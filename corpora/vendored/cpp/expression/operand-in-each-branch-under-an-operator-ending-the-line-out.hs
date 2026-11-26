{-# LANGUAGE CPP #-}

module Data.Array.Lookup ((!?), lookupSpec) where

-- | Returns 'Just' the element of an immutable array at the specified
-- index, or 'Nothing' if the index is out of bounds.
(!?) :: (IArray a e, Ix i) => a i e -> i -> Maybe e
(!?) arr i
  | inRange b i =
      Just
        $ unsafeAt arr
#if MIN_VERSION_base(4,14,0)
        $ unsafeIndex b i
#else
        $ index b i
#endif
  | otherwise = Nothing
  where
    b = bounds arr

lookupSpec :: Spec
lookupSpec = it "reports a name it cannot find" $ do
  (_, actualStderr) <- runHieDb ["name-refs", "$", "GHC.Base"]
  actualStderr
#if MIN_VERSION_base(4,20,0)
    `shouldBe` "Couldn't find name: $ from module GHC.Internal.Base(ghc-internal)\n"
#else
    `shouldBe` "Couldn't find name: $ from module GHC.Base(base)\n"
#endif
