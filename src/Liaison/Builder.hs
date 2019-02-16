-- |
-- Module      :  Liaison.Builder
-- Copyright   :  © 2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Shortcuts for construction of expressions from Haskell.
--
-- For example, expression that is equivalent to this configuration file:
--
-- > { age = 22;
-- >   email = "something@example.org";
-- > }
--
-- can be constructed like this:
--
-- > set [ ("age", int 22)
-- >     , ("email", "something@example.org")
-- >     ]
--
-- The module "Liaison.Printer" has functions that allow us to convert
-- expressions to their textual representation and save them to files.

module Liaison.Builder
  ( str
  , int
  , float
  , set
  )
where

import Data.Coerce
import Data.Functor.Identity
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import Liaison.Expression
import qualified Data.Map as M

-- | Construct a string literal. You can also use Haskell string literals
-- directly if you enable @OverloadedStrings@.

str :: Text -> Exp I
str = String

-- | Construct an integer.

int :: Integral a => a -> Exp I
int = Number . fromIntegral

-- | Construct a floating point number.

float :: RealFloat a => a -> Exp I
float = Number . fromFloatDigits

-- | Construct a set.

set :: [(Text, Exp I)] -> Exp I
set = Set . M.fromList . coerce
