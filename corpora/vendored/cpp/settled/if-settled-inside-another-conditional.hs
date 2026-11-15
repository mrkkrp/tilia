{-# LANGUAGE CPP #-}
#if !defined(javascript_HOST_ARCH)
{-# LANGUAGE CApiFFI #-}
#endif
module Clock where

#ifndef mingw32_HOST_OS
import Foreign.C

#if defined(javascript_HOST_ARCH) || defined(__MHS__)
foreign import ccall unsafe "time.h time" time :: CLong -> IO CLong
#else
foreign import capi unsafe "time.h time" time :: CLong -> IO CLong
#endif

now :: IO CLong
now = time 0
#endif
