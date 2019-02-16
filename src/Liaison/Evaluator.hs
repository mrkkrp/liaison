-- | Evaluation of expressions.

{-# LANGUAGE LambdaCase #-}

module Liaison.Evaluator
  ( evalWhnf
  )
where

import Liaison.Expression

-- | Evaluate 'Exp' to weak head normal form.

evalWhnf
  :: Exp f
  -> NExp (f (Exp f))
evalWhnf = \case
  String txt -> NString txt
  Number x -> NNumber x
  Set m -> NSet m
