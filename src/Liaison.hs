module Liaison
  ( liaisonViaText
  , liaisonViaFile
  )
where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Void
import Liaison.Parser
import Liaison.Validation
import Text.Megaparsec hiding (parse)

-- |

liaisonViaText
  :: ValidaitonError e
  => Parser e a
  -> FilePath
  -> Text
  -> Either (ParseErrorBundle Text (ValidationError e)) a
liaisonViaText p path input =

  case parse path input of
    Left (ParseErrorBundle errs pst) ->
      Left $ ParseErrorBundle (mapParseError absurd <$> errs) pst
    Right lexp ->
      case validate p lexp of
        Left verrs ->
          Left $ ParseErrorBundle (mkFancyError <$> verrs)
        Right x -> Right x

-- |

liaisonViaFile
  :: (MonadIO m, ShowValidationError e)
  => Parser e a
  -> FilePath
  -> m (Either (ParseErrorBundle Text (ValidationError e)) a)
liaisonViaFile p path = do
  r <- parseFile path
  undefined

----------------------------------------------------------------------------
-- Helpers

data

mkFancyError
  :: ShowValidationError e
  => L (ValidationError e)
  -> ParseError s e
mkFancyError = undefined
