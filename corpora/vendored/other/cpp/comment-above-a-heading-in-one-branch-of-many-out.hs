{-# LANGUAGE CPP #-}

module M where

#if MIN_VERSION_thing(9,0,0)
#endif
#if MIN_VERSION_thing(9,0,0)
#ifdef OTHER
#if FLAG
f2 = 2

-- | documentation
#else
f1 = 1
#endif
#else /* OTHER */
-- a remark

-- * a heading

#if FLAG
f2 = 2

-- | documentation
#else
f1 = 1
#endif
#endif /* OTHER */
#else
#ifdef OTHER
#else /* OTHER */
-- a remark
#endif /* OTHER */
#if FLAG
f2 = 2

-- | documentation
#else
f1 = 1
#endif
#endif
