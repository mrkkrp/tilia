{-# LANGUAGE CPP #-}

module Emacs.Module.Text where

extract :: Env -> Value -> IO Text
extract env x = withString env x $ \size bytes ->
#ifdef ASSERTIONS
    do
        -- Leave the terminating NUL out.
        checked <- validate bytes
        decode checked (size - 1)
#ifndef ASSERTIONS
#endif
#else
#ifndef ASSERTIONS
#endif
    case freeze bytes of
        frozen ->
            -- Leave the terminating NUL out.
            pure (decode frozen (size - 1))
#endif
