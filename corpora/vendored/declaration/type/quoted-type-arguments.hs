{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE UnicodeSyntax #-}

plain = describe (Bool)

arrow = describe (Char -> Bool)

quantified = describe (forall k. Holder k)

constrained = describe (Readable r => r)

constrainedTwice = describe ((Readable r, Countable r) => r)

linear = describe (Char %1 -> Bool)

linearUnicode = describe (Char ⊸ Bool)

multiplicity = describe (forall n. Char %n -> Bool)

wrapped =
  describe
    ( ( forall k.
          Holder k
      )
    )

sprawling = describe (forall k n. (Readable k, Countable k)
    => Holder k
    %n -> Maybe
        (Char , Word)
    ⊸ Text)
