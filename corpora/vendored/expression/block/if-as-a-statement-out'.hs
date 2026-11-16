module Stock.Audit where

audit :: Bool -> Int -> IO ()
audit verbose expected = do
    counted <- countShelves
    if verbose then print counted else pure ()
    if counted /= expected
        then warn "the shelves disagree with the ledger"
        else pure ()
    if counted == 0
        then
            warn "nothing on the shelves"
        else
            pure ()
