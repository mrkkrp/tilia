{-# OPTIONS_GHC -XLambdaCase -fbang-patterns #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module Flags where

classify ∷ Int → String
classify = \case
  0 → "zero"
  _ → "other"

strict ∷ Int → Int
strict !n = n

both ∷ (∀ a. a → a) → (Int, Bool)
both f = (f 1, f True)
