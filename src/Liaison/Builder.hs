-- | Shortcuts for construction of expressions from Haskell.

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
import Liaison.Types
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
