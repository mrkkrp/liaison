-- | Evaluation of expressions.

{-# LANGUAGE LambdaCase #-}

module Liaison.Evaluator
  ( eval
  )
where

import Data.Functor.Identity
import Liaison.Types
import qualified Data.Map as M

-- | Transform 'Exp' to its normal form, 'NExp'.

eval
  :: Exp I
  -> NExp
eval = \case
  String txt -> NString txt
  Number x -> NNumber x
  Set m -> NSet (M.map (eval . runIdentity) m)
