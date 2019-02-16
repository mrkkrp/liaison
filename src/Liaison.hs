-- | TODO Overview of the package.

module Liaison
  ( liaisonViaFile
  , liaisonViaText
  , module Liaison.Validation
  )
where

import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Void
import Liaison.Expression
import Liaison.Parser
import Liaison.Validation
import Text.Megaparsec hiding (parse)
import qualified Data.Set as E
import qualified Data.Text.IO as T

-- |

liaisonViaFile
  :: (MonadIO m, ShowValidationError e)
  => V e a
  -> FilePath
  -> m (Either (ParseErrorBundle Text (E (ValidationError e))) a)
liaisonViaFile v path = do
  input <- liftIO (T.readFile path)
  return (liaisonViaText v path input)

-- |

liaisonViaText
  :: ShowValidationError e
  => V e a
  -> FilePath
  -> Text
  -> Either (ParseErrorBundle Text (E (ValidationError e))) a
liaisonViaText v path input =
  case parse path input of
    Left (ParseErrorBundle errs _) ->
      Left $ ParseErrorBundle (mapParseError absurd <$> errs) pst
    Right lexp ->
      case validate v lexp of
        Left verrs ->
          let mkFancyError (L o l verr) =
                FancyError o (E.singleton (ErrorCustom (E l verr)))
          in Left $ ParseErrorBundle (mkFancyError <$> verrs) pst
        Right x -> Right x
  where
    pst = PosState
      { pstateInput = input
      , pstateOffset = 0
      , pstateSourcePos = initialPos path
      , pstateTabWidth = defaultTabWidth
      , pstateLinePrefix = ""
      }
