{-# LANGUAGE CPP #-}
#ifdef FAST
{-# LANGUAGE MagicHash #-}
#endif

module Terminal.Same (same) where

#ifdef FAST
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
#endif

same :: a -> a -> Bool
#ifdef FAST
same x y = isTrue# (reallyUnsafePtrEquality# x y)
#else
same _ _ = False
#endif
{-# INLINE same #-}

infix 4 `same`
