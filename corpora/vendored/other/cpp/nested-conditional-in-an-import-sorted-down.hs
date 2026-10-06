{-# LANGUAGE CPP #-}
module Terminal.Mask where

import Control.Exception hiding
    ( throwIO
#if MIN_VERSION_base(4,3,0)
    , mask
#if MIN_VERSION_base(4,4,0)
    , allowInterrupt
#endif
#else
    , block
#endif
    )
import qualified Control.Concurrent as C
