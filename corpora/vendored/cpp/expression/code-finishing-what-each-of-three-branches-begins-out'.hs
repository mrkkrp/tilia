{-# LANGUAGE CPP #-}

module Mill.Grind where

grind :: Int -> IO Int
grind kilos = do
#if defined(STONE_MILL)
    flour <-
        stoneGrind
            (sieve kilos)
#elif defined(ROLLER_MILL)
    flour <-
        rollerGrind
            (sieve kilos)
#else
    flour <-
        handGrind
            (sieve kilos)
#endif
            coarse
            fine
    pure flour

grades :: [(Int, String)] -> [String]
#if defined(STONE_MILL)
grades xs =
    [ show n ++ s
#elif defined(ROLLER_MILL)
grades xs =
    [ s ++ show n
#else
grades xs =
    [ s
#endif
    | (n, s) <- xs
    ]
