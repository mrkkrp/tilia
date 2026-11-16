{-# LANGUAGE CPP #-}

module Kitchen.Scale (weigh) where

weigh :: Scale -> Int -> Int
weigh scale grams =
#if defined(CHECKED)
    assert (grams >= 0) $
#endif
    let n = reading scale
     in n + grams
