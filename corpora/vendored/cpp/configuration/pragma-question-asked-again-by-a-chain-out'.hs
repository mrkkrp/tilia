{-# LANGUAGE CPP #-}
#if defined(HAVE_INTERPRETER) && defined(CAN_LOAD)
{-# LANGUAGE LambdaCase #-}
#endif

module Kiln.Loader (load) where

#if defined(HAVE_INTERPRETER) && defined(CAN_LOAD)
import Data.Maybe (fromMaybe)
#endif

load :: Maybe Int -> Int
#if !defined(HAVE_INTERPRETER)
load _ = 0
#elif !defined(CAN_LOAD)
load _ = 1
#else
load = \case
    Nothing -> 2
    Just n -> fromMaybe n Nothing
#endif
