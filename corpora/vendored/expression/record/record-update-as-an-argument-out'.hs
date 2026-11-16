module Kiln.Batches where

firstBatch :: Maybe Batch
firstBatch =
    Just
        -- the one already loaded
        current
            { batchSize = 12
            }

emptied :: Maybe Batch
emptied = Just current{batchSize = 0}
