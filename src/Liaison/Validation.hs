{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE EmptyCase           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Consumption of expressions from host language.

module Liaison.Validation
  ( -- * Types
    V
  , validate
  , ValidationError
  , ShowValidationError (..)
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

import Data.Data (Data, dataTypeOf, dataTypeName)
import Data.List.NonEmpty (NonEmpty)
import Data.Scientific hiding (scientific)
import Data.Text (Text)
import Data.Void
import Liaison.Expression
import Text.Megaparsec

-- |

newtype V e a = V
  { runV :: (L (Exp L) -> Either (NonEmpty (L (ValidationError e))) a)
  } deriving Functor

-- |

validate
  :: V e a
  -> L (Exp L)
  -> Either (NonEmpty (L (ValidationError e))) a
validate = undefined

-- |

data ValidationError e
  = ExpectedString NExp
  | ExpectedInteger NExp
  | IntegerOutOfRange String NExp
  | ExpectedFloat NExp
  | ExpectedNumber NExp
  | OtherValidationError e
  deriving (Eq, Ord, Show)

-- |

class Ord e => ShowValidationError e where
  showValidaitonError :: e -> String

instance ShowValidationError e
    => ShowErrorComponent (E (ValidationError e)) where
  showErrorComponent (E _ e) =
    let f expected nexp =
          "expected " ++ expected ++ ", but got " ++ nexpName nexp
    in case  e of
        ExpectedString x -> f "string" x
        ExpectedInteger x -> f "integer" x
        IntegerOutOfRange t x -> f ("integer fitting range of " ++ t) x
        ExpectedFloat x -> f "float" x
        ExpectedNumber x -> f "number" x
        OtherValidationError x -> showValidaitonError x
  errorComponentLen (E len _) = len

----------------------------------------------------------------------------
-- Primitive combinators

consume :: (NExp -> Either (ValidationError Void) a) -> V e a
consume = undefined

index :: Text -> V e a -> V e a
index = undefined

check :: (s -> Either e a) -> V e s -> V e a
check = undefined

----------------------------------------------------------------------------
-- Derivative combinators

str :: V e Text
str = consume $ \case
  NString txt -> Right txt
  nexp -> Left (ExpectedString nexp)

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

float :: RealFloat a => V e a
float = consume $ \case
  NNumber x ->
    if isInteger x
      then Left (ExpectedFloat (NNumber x))
      else Right (toRealFloat x)
  nexp -> Left (ExpectedFloat nexp)

scientific :: V e Scientific
scientific = consume $ \case
  NNumber x -> Right x
  nexp -> Left (ExpectedNumber nexp)
