module Shop.Order where

data Order
    = Phone
        -- | The caller's number.
        String
        -- | How many items.
        Int
    | Counter
        -- | The till.
        !Int
        Bool
    | -- | Bought online.
      Online
        -- | The account the order is billed to.
        String

data Line = Line {- | The quantity. -} Int Double
