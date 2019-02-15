{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Types used in the library.

module Liaison.Types
  ( Exp (..)
  , mapExp
  , L (..)
  , unL
  , I
  , NExp (..)
  )
where

import Data.Functor.Identity
import Data.Map (Map)
import Data.Scientific (Scientific)
import Data.String
import Data.Text (Text)
import qualified Data.Map as M

-- | Expression type, result of parsing. When it's evaluated, 'NExp' is
-- produced. @f@ is an annotation wrapper.

data Exp f
  = String Text                 -- ^ String literals
  | Number Scientific           -- ^ Numeric literals (both integers and floats)
  | Set (Map Text (f (Exp f)))  -- ^ Sets

-- | Map annotation wrapper.

mapExp :: Functor g => (forall a. f a -> g a) -> Exp f -> Exp g
mapExp f = \case
  String txt -> String txt
  Number x -> Number x
  Set m -> Set (M.map (fmap (mapExp f) . f) m)

deriving instance (forall a. Eq a => Eq (f a)) => Eq (Exp f)
deriving instance (forall a. Show a => Show (f a)) => Show (Exp f)

-- | Location information. The first 'Int' is offset, the second 'Int' is
-- length in characters.

data L a = L Int Int a
  deriving (Eq, Show, Functor)

-- | Drop location info.

unL :: L a -> a
unL (L _ _ a) = a

-- | A synonym for 'Identity'.

type I = Identity

-- | Normal form of expressions.

data NExp
  = NString Text                -- ^ Strings
  | NNumber Scientific          -- ^ Numbers
  | NSet (Map Text NExp)        -- ^ Sets
  deriving (Eq, Show)

instance IsString NExp where
  fromString = NString . fromString
