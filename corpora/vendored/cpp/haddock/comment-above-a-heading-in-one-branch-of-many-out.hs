{-# LANGUAGE CPP #-}

module M where

#if MIN_VERSION_thing(9,0,0)
#endif
#ifdef OTHER
#else /* OTHER */
-- a remark
#if MIN_VERSION_thing(9,0,0)
-- * a heading
#endif
#endif /* OTHER */
#if FLAG
f2 = 2

-- | documentation
#else
f1 = 1
#endif
