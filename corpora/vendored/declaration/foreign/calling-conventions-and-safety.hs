{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE JavaScriptFFI #-}

module Audio.Device where

import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr)

type Handler = CInt -> IO ()

foreign import ccall safe "audio_open" openDevice :: CInt -> IO CInt

foreign import ccall interruptible "audio_wait" waitDevice :: Handler

foreign import stdcall "audio_close" closeDevice :: Handler

foreign import capi "audio.h AUDIO_RATE" defaultRate :: CInt

foreign import javascript "navigator.audioDevices" deviceCount :: IO Int

foreign import ccall "dynamic" runHandler :: FunPtr Handler -> Handler

foreign import ccall "wrapper" wrapHandler :: Handler -> IO (FunPtr Handler)
