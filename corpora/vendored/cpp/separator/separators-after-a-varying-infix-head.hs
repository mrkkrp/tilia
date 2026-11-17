{-# LANGUAGE CPP #-}

module Audio.Mix where

-- An equation and a case alternative whose head is written infix, with
-- the operator in each branch and what follows it written once.
x
#if MIN_VERSION_base(4,18,0)
  <->
#else
  `fade`
#endif
  y = unmix x y

loudness :: Tracks -> Int
loudness tracks = case tracks of
  left
#if MIN_VERSION_base(4,18,0)
    :|
#else
    :<
#endif
    right -> peak left right
