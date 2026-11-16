module Audio.Mixer where

import Foreign.C.Types (CFloat (..), CInt (..))

foreign import ccall unsafe "mixer_gain"
  mixerGain :: CInt -> CFloat -> IO ()

foreign import ccall "mixer_level" mixerLevel
  :: CInt -> IO CFloat

foreign import ccall "mixer_route"
  mixerRoute ::
    CInt ->
    CInt ->
    IO CInt

foreign export ccall "mixer_version"
  mixerVersion :: CInt

foreign export ccall mixerReset
  :: CInt -> IO ()

mixerVersion :: CInt
mixerVersion = 3

mixerReset :: CInt -> IO ()
mixerReset _ = pure ()
