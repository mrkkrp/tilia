{-# LANGUAGE CPP #-}

module Test.State where

state :: State
state =
  MkState
    { terminal = undefined,
      coverageConfidence = undefined,
#if MIN_VERSION_QuickCheck(2,15,0)
      maxTestSize = 0,
      replayStartSize = undefined,
#else
      computeSize = undefined,
#endif
      numTotMaxShrinks = 0,
      expected = True
    }
