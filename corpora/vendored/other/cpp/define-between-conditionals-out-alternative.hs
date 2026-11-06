{-# LANGUAGE CPP #-}

module M where

-- | documentation
#if FLAG
-- a remark
f9 = 9
#endif
#define WIDE 1
#if FLAG
-- a remark
#endif
