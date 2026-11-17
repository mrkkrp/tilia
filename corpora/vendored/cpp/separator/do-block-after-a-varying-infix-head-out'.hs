{-# LANGUAGE CPP #-}

module Audio.Mix where

-- An equation and a case alternative whose head is written infix, with
-- the operator in each branch and a do block hanging after the separator.
x
#if MIN_VERSION_base(4,18,0)
    <-> y =
#else
    `fade` y =
#endif
        do
            unmix x y

loudness :: Tracks -> IO Int
loudness tracks = case tracks of
    left
#if MIN_VERSION_base(4,18,0)
        :| right ->
#else
        :< right ->
#endif
            do
                peak left right
