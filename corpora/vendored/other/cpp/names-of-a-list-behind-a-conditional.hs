{-# LANGUAGE CPP #-}
module Terminal.Signals where

import Data.List
    ( sortOn, nub
#if MIN_VERSION_base(4,8,0)
    , foldl1
#endif
    , group
    )
import System.Posix.Signals
    ( installHandler, Handler(..)
#if MIN_VERSION_unix(2,7,0)
    , raiseSignal, sigINT
#if MIN_VERSION_unix(2,8,0)
    , sigWINCH
#endif
#else
    , signalProcess, sigTERM
#endif
    , emptySignalSet, blockSignals
    )
