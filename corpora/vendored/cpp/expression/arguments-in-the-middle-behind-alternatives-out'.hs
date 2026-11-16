{-# LANGUAGE CPP #-}

module Kitchen.Shelf (stock) where

stock :: Pantry -> IO Shelf
stock pantry = do
    items <- gather pantry
    let shelf =
            Shelf
                []
#ifdef WITH_LABELS
                Nothing
                labelName
#else
                defaultLabel
                [kind pantry]
#endif
                Nothing
                items
                extras
    pure shelf
