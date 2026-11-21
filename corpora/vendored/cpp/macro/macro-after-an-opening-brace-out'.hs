{-# LANGUAGE CPP #-}

module M where

#define SMALL(n) n -> True;
#define DOUBLED(x) doubled = x * 2;

isSmall :: Int -> Bool
isSmall w = case w of
    {
      SMALL(0)
      SMALL(1)
      _ -> False
    }

isTiny :: Int -> Bool
isTiny w = case w of
    {
#define TINY(n) n -> True;
      TINY(0)
#undef TINY
      _ -> False
    }

quadrupled :: Int -> Int
quadrupled x =
    let {
          DOUBLED(x)
          twice = doubled * 2 }
     in twice

octupled :: Int -> Int
octupled x = thrice
    where
        {
          DOUBLED(x)
          thrice = doubled * 4 }
