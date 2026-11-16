module Inventory.Restock where

restock :: Item -> IO ()
restock item = do
    supplier <- case itemOrigin item of
        Local -> nearestDepot
        Abroad country -> importer country
    amount <-
        case itemStock item of
            0 -> pure 100
            n -> pure (100 - n)
    order supplier item amount
