{-# LANGUAGE CPP #-}

module M where

#if MIN_VERSION_thing(9,0,0)
-- * a heading

f8 = 8

#if MIN_VERSION_thing(1,0,0)
#else
-- a remark

-- * a heading
#endif
#else
-- * a heading
#if MIN_VERSION_thing(1,0,0)
#else
-- a remark
#endif
#endif
