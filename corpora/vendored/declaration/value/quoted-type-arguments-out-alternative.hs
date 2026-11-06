{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UnicodeSyntax #-}

plain = describe (Bool)

arrow = describe (Char → Bool)

quantified = describe (∀ k. Holder k)

constrained = describe ((Readable r) ⇒ r)

constrainedTwice = describe ((Readable r, Countable r) ⇒ r)

linear = describe (Char ⊸ Bool)

linearUnicode = describe (Char ⊸ Bool)

multiplicity = describe (∀ n. Char %n → Bool)

wrapped =
  describe
    ( ( ∀ k.
        Holder k
      )
    )

sprawling =
  describe
    ( ∀ k n.
      (Readable k, Countable k) ⇒
      Holder k %n →
      Maybe
        (Char, Word) ⊸
      Text
    )
