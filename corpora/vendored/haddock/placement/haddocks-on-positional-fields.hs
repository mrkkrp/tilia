module Shop.Order where

data Order
  = Phone
      String -- ^ The caller's number.
      Int -- ^ How many items.
  | Counter
      !Int -- ^ The till.
      Bool
  | -- | Bought online.
    Online String -- ^ The account the order is billed to.

data Line = Line {- | The quantity. -} Int Double
