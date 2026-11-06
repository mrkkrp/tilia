{-# LANGUAGE Arrows #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- Where UnicodeSyntax is in force, every symbol that has a Unicode form
-- comes out in it, whichever form it was written in.
module Garden.Beds where

apply ∷ ∀ a b. (Show a) ⇒ (a → b) → a → IO b
apply f x = do
  y ← pure (f x)
  print x
  pure y

none ∷ ∀ . Int
none = 1

data Bed (f ∷ ★ → ★) = Bed

seal ∷ a ⊸ a
seal x = x

class Plot a b | a → b where
  plant ∷ a → b

data Row where
  Row ∷ Int ⊸ Row
  Named ∷ {rowName ∷ Int} → Row

pattern First x ← x : _

measure (length → n) = n

water f = proc x → do
  y ← f ⤙ x
  f ⤛ y

tend f g = proc x → ⦇ (<+>) (f ⤙ x) (g ⤙ x) ⦈

spring = ⟦1 + 2⟧

summer = [e|3|]

autumn = [d|frost = 1|]
