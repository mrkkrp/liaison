{-# LANGUAGE DeriveFunctor #-}

-- | Consumption of expressions from host language.

module Liaison.Validation
  ( -- * Types
    Parser
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

import Data.List.NonEmpty (NonEmpty)
import Data.Scientific hiding (scientific)
import Data.Text (Text)
import Data.Void
import Liaison.Types

-- |

newtype Parser e a = Parser
  { runParser :: (L (Exp L) -> Either (NonEmpty (L (ValidationError e))) a)
  } deriving (Functor)

-- |

validate
  :: Parser e a
  -> L (Exp L)
  -> Either (NonEmpty (L (ValidationError e))) a
validate = undefined

-- |

data ValidationError e
  = ExpectedString
  | ExpectedInteger
  | ExpectedFloat
  | Other e
  deriving (Eq, Show)

class ShowValidationError e where
  showValidaitonError :: e -> String

instance ShowValidaitonError e => ShowErrorComponent (L (ValidationError e)) where
  showErrorComponent = undefined
  errorComponentLen (L _ len _) = len

----------------------------------------------------------------------------
-- Primitive combinators

consume :: (NExp -> Either (ValidationError Void) a) -> Parser e a
consume = undefined

index :: Text -> Parser e a -> Parser e a
index = undefined

check :: (s -> Either e a) -> Parser e s -> Parser e a
check = undefined

----------------------------------------------------------------------------
-- Derivative combinators

str :: Parser e Text
str = undefined

int :: Integral a => Parser e a
int = undefined

float :: RealFloat a => Parser e a
float = undefined

scientific :: Parser e Scientific
scientific = undefined
