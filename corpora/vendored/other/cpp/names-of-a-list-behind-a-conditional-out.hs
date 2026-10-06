{-# LANGUAGE CPP #-}

module Terminal.Signals where

import Data.List
  ( nub,
    sortOn,
#if MIN_VERSION_base(4,8,0)
    foldl1,
#endif
    group,
  )
import System.Posix.Signals
  ( Handler (..),
    installHandler,
#if MIN_VERSION_unix(2,7,0)
    raiseSignal,
    sigINT,
#if MIN_VERSION_unix(2,8,0)
    sigWINCH,
#endif
#else
    sigTERM,
    signalProcess,
#endif
    blockSignals,
    emptySignalSet,
  )
