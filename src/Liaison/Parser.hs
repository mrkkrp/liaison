-- | This module defines how to parse expressions of the configuration
-- language.

module Liaison.Parser
  ( parse
  , parseFile
  )
where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Void
import Liaison.Types
import Text.Megaparsec hiding (parse)
import qualified Data.Text.IO as T

-- | Parse an expression form 'Text'.

parse
  :: FilePath                   -- ^ File name
  -> Text                       -- ^ Input
  -> Either (ParseErrorBundle Text Void) (L (Exp L))
parse = undefined -- TODO

-- | Parse an expression form a file on disk.

parseFile
  :: MonadIO m
  => FilePath
  -> m (Either (ParseErrorBundle Text Void) (L (Exp L)))
parseFile path =
  parse path <$> liftIO (T.readFile path)
