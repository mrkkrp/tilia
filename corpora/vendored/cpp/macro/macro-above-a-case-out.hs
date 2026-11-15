{-# LANGUAGE CPP #-}

module Bounds.Check where

#if defined(ASSERTS)
#define CHECK(i) if (i) < 0 then error "negative index" else
#else
#define CHECK(i)
#endif

pick :: Int -> String
pick i =
  CHECK(i)
  case i of
    0 -> "zero"
    _ -> "more"
