module Circuit.Native where

import Foreign.C.Types (CInt (..))

foreign import ccall "wire_join" (<+>) :: CInt -> CInt -> CInt
