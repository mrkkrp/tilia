{-# LANGUAGE CPP #-}

module Emacs.Module.Common where

checked :: Env -> Value -> IO Value
checked env x = withValue env x $ \y ->
#ifdef CHECKED
    check y
#endif
#ifndef CHECKED
    y
#endif
