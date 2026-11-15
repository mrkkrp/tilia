{-# LANGUAGE CPP #-}
#ifndef mingw32_HOST_OS
{-# LANGUAGE CApiFFI #-}
#endif
module Process.Self where

#ifdef mingw32_HOST_OS
import System.Win32.Process (getCurrentProcessId)
#endif
#ifndef mingw32_HOST_OS
import Foreign.C.Types (CInt (..))
#endif

selfId :: IO Int
#ifdef mingw32_HOST_OS
selfId  =  fromIntegral <$> getCurrentProcessId
#endif
#ifndef mingw32_HOST_OS
selfId  =  fromIntegral <$> getPid

foreign import capi "unistd.h getpid" getPid :: IO CInt
#endif
