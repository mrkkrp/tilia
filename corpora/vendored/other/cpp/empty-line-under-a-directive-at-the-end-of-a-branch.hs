{-# LANGUAGE CPP #-}

module Route.Vehicle where

#if defined(WITH_TRAMS)
trams :: Int
trams = 1
#define TRAMS 1

#endif
