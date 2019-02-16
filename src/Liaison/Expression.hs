{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Expression definitions.

module Liaison.Expression
  ( Exp (..)
  , mapExp
  , eraseL
  , L (..)
  , unL
  , E (..)
  , unE
  , I
  , NExp (..)
  , nexpName
  )
where

import Data.Functor.Identity
import Data.Map (Map)
import Data.Scientific (Scientific, isInteger)
import Data.String
import Data.Text (Text)
import qualified Data.Map as M

-- | Expression type, result of parsing. When it's evaluated, 'NExp' is
-- produced. @f@ is an annotation wrapper.

data Exp f
  = String Text                 -- ^ String literals
  | Number Scientific           -- ^ Numeric literals (both integers and floats)
  | Set (Map Text (f (Exp f)))  -- ^ Sets

deriving instance (forall a. Eq a => Eq (f a)) => Eq (Exp f)
deriving instance ( (forall a. Eq a => Eq (f a))
                  , (forall a. Ord a => Ord (f a))
                  ) => Ord (Exp f)
deriving instance (forall a. Show a => Show (f a)) => Show (Exp f)

-- | String literals can be used to write string expressions directly if
-- @OverloadedStrings@ is enabled.

instance IsString (Exp f) where
  fromString = String . fromString

-- | Map annotation wrapper.

mapExp
  :: Functor g
  => (forall a. f a -> g a)     -- ^ Natural transformation to apply
  -> Exp f                      -- ^ Original expression tree
  -> Exp g                      -- ^ Resulting expression tree
mapExp f = \case
  String txt -> String txt
  Number x -> Number x
  Set m -> Set (M.map (fmap (mapExp f) . f) m)

-- | Erase location information from AST.
--
-- > eraseL = mapExp (Identity . unL)

eraseL :: Exp L -> Exp I
eraseL = mapExp (Identity . unL)

-- | Location information. The first 'Int' is offset, the second 'Int' is
-- length in characters.

data L a = L Int Int a
  deriving (Eq, Ord, Show, Functor)

-- | Drop location info.

unL :: L a -> a
unL (L _ _ a) = a

-- | Reduced version of 'L' that only contains length in characters without
-- offset.

data E a = E Int a
  deriving (Eq, Ord, Show, Functor)

-- | Drop length info.

unE :: E a -> a
unE (E _ a) = a

-- | A synonym for 'Identity'.

type I = Identity

-- | 'Exp' with its outer layer evaluated.

data NExp e
  = NString Text                -- ^ Strings
  | NNumber Scientific          -- ^ Numbers
  | NSet (Map Text e)           -- ^ Sets
  deriving (Eq, Ord, Show)

-- | Get textual description of given 'NExp'.

nexpName :: NExp e -> String
nexpName = \case
  NString _ -> "string"
  NNumber x ->
    if isInteger x
      then "integer"
      else "float"
  NSet _ -> "set"
