{-# LANGUAGE CPP #-}

module Route.Night where

surcharge :: Int -> Int
surcharge fare =
  let base = fare + 1
      -- night buses cost more where they run
   in
#ifdef NIGHT_SERVICE
      base * 2
#else
      base
#endif
