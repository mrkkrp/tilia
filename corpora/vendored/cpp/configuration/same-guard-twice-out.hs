{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Terminal.Run (runFrame) where

import Control.Monad.ST (ST, runST)
import Terminal.Buffer (Buffer, freeze, shrink)
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif

runFrame ::
#if defined(ASSERTS)
  (HasCallStack) =>
#endif
  (forall s. (Buffer s -> Int -> ST s Buffer) -> ST s Buffer) -> Buffer
runFrame act = runST (act (\buffer used -> shrink buffer used >> freeze buffer))
