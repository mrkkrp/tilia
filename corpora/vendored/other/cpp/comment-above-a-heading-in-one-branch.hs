{-# LANGUAGE CPP #-}

module M where

#if MIN_VERSION_thing(9,0,0)
-- * a heading
f8 = 8
#else
-- * a heading
#endif
#if MIN_VERSION_thing(1,0,0)
#else
-- a remark
#if MIN_VERSION_thing(9,0,0)
-- * a heading
#endif
#endif
