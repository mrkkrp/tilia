module Invoice.Total where

total :: [Item] -> Money
total items = sum (map _price items) + _

discounted :: Rate -> [Item] -> Money
discounted rate = _applyRate rate . total
