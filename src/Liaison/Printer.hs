-- |
-- Module      :  Liaison.Printer
-- Copyright   :  © 2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Rendering of expressions.

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Liaison.Printer
  ( printText
  , printTextL
  , printBuilder
  )
where

import Data.Text (Text)
import Data.Text.Lazy.Builder (Builder)
import Liaison.Expression
import Prelude hiding (print)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Scientific as B

-- | Print an expression as 'Text'.

printText :: Exp I -> Text
printText = TL.toStrict . printTextL

-- | Print an expression as lazy 'Text'.

printTextL :: Exp I -> TL.Text
printTextL = B.toLazyText . printBuilder

-- | Print an expression as 'Builder'.

printBuilder :: Exp I -> Builder
printBuilder = \case
  String txt -> "\"" <> B.fromText (escapeString txt) <> "\""
  Number x -> B.scientificBuilder x
  Set _m -> "{" <> "too challenging" <> "}" -- TODO

-- |

escapeString :: Text -> Text
escapeString = undefined -- TODO
