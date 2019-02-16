-- |
-- Module      :  Liaison.Parser
-- Copyright   :  © 2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module defines how to parse expressions of the configuration
-- language.

module Liaison.Parser
  ( parseFile
  , parse
  )
where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Void
import Liaison.Expression
import Text.Megaparsec hiding (parse)
import qualified Data.Text.IO as T

-- | Parse an expression form a file on disk.

parseFile
  :: MonadIO m
  => FilePath                   -- ^ Location of file to parse
  -> m (Either (ParseErrorBundle Text Void) (L (Exp L)))
parseFile path =
  parse path <$> liftIO (T.readFile path)

-- | Parse an expression form 'Text'.

parse
  :: FilePath                   -- ^ File name
  -> Text                       -- ^ Input
  -> Either (ParseErrorBundle Text Void) (L (Exp L))
parse = undefined -- TODO
