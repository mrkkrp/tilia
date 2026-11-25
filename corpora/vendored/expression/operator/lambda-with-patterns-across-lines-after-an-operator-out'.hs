-- A lambda whose patterns take several lines does not hang after the
-- operator before it, as one whose patterns take one line does.
module Shop.Receipt where

lines' :: [Order] -> [Line]
lines' orders =
    flip map orders $
        \Order
            { orderId = number
            , orderTotal = total
            } ->
            Line number total
