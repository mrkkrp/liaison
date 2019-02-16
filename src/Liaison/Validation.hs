-- |
-- Module      :  Liaison.Validation
-- Copyright   :  © 2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module describes how to do consumption of expressions from host
-- language.
--
-- Typically you will have a data type which contains the configuration
-- options you want to load. There may be more than one way to map form
-- parsed expression to Haskell data type, thus we do not provide type class
-- for this, instead use 'V' to create a parser, or as we call it,
-- /validator/.
--
-- An example of validator follows:
--
-- > data User = User
-- >   { userAge :: Maybe Int
-- >   , userEmail :: Text
-- >   }
-- >
-- > vUser :: V Void User
-- > vUser = User
-- >   <$> optional (index "age" int)
-- >   <*> index "email" str
--
-- In this example @vUser@ could be used to consume a configuration such as
-- this:
--
-- > { age = 22;
-- >   email = "something@example.org";
-- > }

{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Liaison.Validation
  ( -- * Types and re-exports
    V
  , validate
  , ValidationError (..)
  , ShowValidationError (..)
  , module Data.Functor.Alt
  , module Liaison.Expression
    -- * Primitive combinators
  , consume
  , index
  , check
    -- * Derivative combinators
  , str
  , int
  , float
  , scientific
  )
where

import Data.Bifunctor
import Data.Data (Data, dataTypeOf, dataTypeName)
import Data.Functor.Alt
import Data.List.NonEmpty (NonEmpty (..))
import Data.Scientific hiding (scientific)
import Data.Text (Text)
import Data.Void
import Liaison.Evaluator
import Liaison.Expression
import Text.Megaparsec hiding (optional)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T

-- | Validator. Use primitives like 'consume', 'index', and 'check' as basis
-- for building your validators. Note that 'V' is an instance of 'Functor',
-- 'Applicative', and 'Alt' (which is like 'Control.Alternative.Alternative'
-- but without 'Control.Alternative.empty').

newtype V e a = V
  { runV :: L (Exp L) -> Either (NonEmpty (L (ValidationError e))) a
  } deriving Functor

instance Applicative (V e) where
  pure x = V $ \_ -> Right x
  (V f) <*> (V x) = V $ \e ->
    case (f e, x e) of
      (Left errs0, Left errs1) -> Left (errs0 <> errs1)
      (Left errs0, Right _) -> Left errs0
      (Right _, Left errs1) -> Left errs1
      (Right f', Right x') -> Right (f' x')

instance Alt (V e) where
  V x <!> V y = V $ \e ->
    case (x e, y e) of
      (Left errs0, Left errs1) -> Left (errs0 <> errs1)
      (Left _, Right y') -> Right y'
      (Right x', _) -> Right x'

-- | Run a validator on given expression.

validate
  :: V e a                      -- ^ Validator to run
  -> L (Exp L)                  -- ^ Expression to consume
  -> Either (NonEmpty (L (ValidationError e))) a
validate v = first (NE.sortBy f) . runV v
  where
    f (L o0 _ _) (L o1 _ _) = o0 `compare` o1

-- | Validation errors.

data ValidationError e
  = ExpectedString WExp         -- ^ Expected a string
  | ExpectedInteger WExp        -- ^ Expected an integer
  | IntegerOutOfRange String WExp -- ^ Number that comes from configuration
                                -- file is too big or too small for the
                                -- Haskell type you're trying to store it in
  | ExpectedFloat WExp          -- ^ Expected a floating point number
  | ExpectedNumber WExp         -- ^ Expected a number
  | NoKeyInSet Text WExp        -- ^ Tried to access using a key which is
                                -- not there in the set
  | ExpectedSet WExp            -- ^ Expected a set
  | OtherValidationError e      -- ^ User-defined validation errors
  deriving (Eq, Ord, Show, Functor)

-- | This type class defines how to pretty-print user-defined validation
-- errors.

class Ord e => ShowValidationError e where

  -- | Map validation error to 'Prelude.String' to show it to the user.
  showValidaitonError :: e -> String

instance ShowValidationError e
    => ShowErrorComponent (E (ValidationError e)) where
  showErrorComponent (E _ e) =
    let f expected nexp =
          "expected " ++ expected ++ ", but got " ++ nexpName nexp
    in case e of
        ExpectedString x -> f "string" x
        ExpectedInteger x -> f "integer" x
        IntegerOutOfRange t x -> f ("integer fitting range of " ++ t) x
        ExpectedFloat x -> f "float" x
        ExpectedNumber x -> f "number" x
        NoKeyInSet key _ ->
          "the set does not contain key \"" ++ T.unpack key ++ "\""
        ExpectedSet x -> f "set" x
        OtherValidationError x -> showValidaitonError x
  errorComponentLen (E len _) = len

----------------------------------------------------------------------------
-- Primitive combinators

-- | Consume an expression in normal form.

consume
  :: (WExp -> Either (ValidationError Void) a)
     -- ^ Consuming function
  -> V e a
consume f = V $ \(L o l e) ->
  case f (evalWhnf e) of
    Left err -> Left ((L o l (absurd <$> err)) :| [])
    Right x -> Right x

-- | Index a set by given key.

index
  :: Text                       -- ^ Key
  -> V e a                      -- ^ How to validate corresponding value
  -> V e a                      -- ^ Validator for a set
index key (V f) = V $ \(L o l e) ->
  case evalWhnf e of
    NSet m ->
      case M.lookup key m of
        Nothing -> Left ((L o l (NoKeyInSet key (NSet m))) :| [])
        Just e' -> f e'
    nexp -> Left ((L o l (ExpectedSet nexp)) :| [])

-- | Run additional checking on a value.

check
  :: (s -> Either e a)          -- ^ Checking function
  -> V e s                      -- ^ Original validator
  -> V e a                      -- ^ Resulting validator
check f (V g) = V $ \(L o l e) ->
  case g (L o l e) of
    Left errs -> Left errs
    Right x ->
      case f x of
        Left err -> Left ((L o l (OtherValidationError err)) :| [])
        Right x' -> Right x'

----------------------------------------------------------------------------
-- Derivative combinators

-- | Expect a string.

str :: V e Text
str = consume $ \case
  NString txt -> Right txt
  nexp -> Left (ExpectedString nexp)

-- | Expect a bounded integer.

int :: forall e a. (Data a, Integral a, Bounded a) => V e a
int = consume $ \case
  NNumber x ->
    if isInteger x
      then case toBoundedInteger x of
             Nothing -> Left (IntegerOutOfRange t (NNumber x))
             Just i -> Right i
      else Left (ExpectedInteger (NNumber x))
  nexp -> Left (ExpectedInteger nexp)
  where
    t = dataTypeName (dataTypeOf (undefined :: a))

-- | Expect a floating point number.

float :: RealFloat a => V e a
float = consume $ \case
  NNumber x ->
    if isInteger x
      then Left (ExpectedFloat (NNumber x))
      else Right (toRealFloat x)
  nexp -> Left (ExpectedFloat nexp)

-- | Expect a number.

scientific :: V e Scientific
scientific = consume $ \case
  NNumber x -> Right x
  nexp -> Left (ExpectedNumber nexp)
