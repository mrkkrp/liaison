{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Printing of expressions.

module Liaison.Printer
  ( print
  )
where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Liaison.Expression
import Prelude hiding (print)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Scientific as B

-- | Print an expression.

print
  :: Exp I                      -- ^ Expression to print
  -> Builder
print = \case
  String txt -> "\"" <> B.fromText (escapeString txt) <> "\""
  Number x -> B.scientificBuilder x
  Set _m -> "{" <> "too challenging" <> "}" -- TODO

-- |

escapeString :: Text -> Text
escapeString = undefined -- TODO
