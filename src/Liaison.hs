-- |
-- Module      :  Liaison
-- Copyright   :  © 2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Liaison is a subset of Nix that works as a configuration language. For
-- simple consumption of configurations use 'liaisonViaFile' and
-- 'liaisonViaText'. These functions combine parsing step and validation
-- step for you merging the errors for simplicity of use.
--
-- Validators are described in the "Liaison.Validation" module, which we
-- re-export here. "Liaison.Builder" provides combinators for type-safe
-- construction of Liaison expressions. They then can be serialized with
-- help of the functions from "Liaison.Printer". Other modules probably
-- won't be interesting for end users.

module Liaison
  ( liaisonViaFile
  , liaisonViaText
  , module Liaison.Expression
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

-- | Parse configuration from a file and validate it with given validator.
--
-- Errors can be printed with 'errorBundlePretty' given that @e@ is an
-- instance of 'ShowValidationError'.

liaisonViaFile
  :: (MonadIO m, ShowValidationError e)
  => V e a                      -- ^ Validator to use
  -> FilePath                   -- ^ Location of configuration file
  -> m (Either (ParseErrorBundle Text (E (ValidationError e))) a)
liaisonViaFile v path = do
  input <- liftIO (T.readFile path)
  return (liaisonViaText v path input)

-- | Similar to 'liaisonViaFile', but takes input directly as 'Text'.

liaisonViaText
  :: ShowValidationError e
  => V e a                      -- ^ Validator to use
  -> FilePath                   -- ^ File name (only used in error messages)
  -> Text                       -- ^ Input
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
