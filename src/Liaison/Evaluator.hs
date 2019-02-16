-- |
-- Module      :  Liaison.Evaluator
-- Copyright   :  © 2019 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Evaluation of expressions.

{-# LANGUAGE LambdaCase #-}

module Liaison.Evaluator
  ( evalWhnf
  )
where

import Liaison.Expression

-- | Evaluate 'Exp' to weak head normal form. Typically we don't evaluate
-- more than necessary, so this function just “reduces” the outermost
-- construction which can then be inspected and consumed. The rest of the
-- tree remains unevaluated.

evalWhnf
  :: Exp f
  -> NExp (f (Exp f))
evalWhnf = \case
  String txt -> NString txt
  Number x -> NNumber x
  Set m -> NSet m
