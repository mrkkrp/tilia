{-# LANGUAGE CPP #-}

module Test.Main where

tests :: [Test]
tests =
    [unitTests, propertyTests]
        ++
#ifndef RELY_ON_SYSTEM_INSTALL
        [scriptTests]

-- | The tests that run the script from the source tree.
scriptTests :: Test
scriptTests = testGroup "script" []
#else
            []
#endif
