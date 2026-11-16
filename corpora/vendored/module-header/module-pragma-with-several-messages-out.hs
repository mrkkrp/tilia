module Ledger.Journal.Old
  {-# DEPRECATED
    [ "Entries are kept in Ledger.Journal now.",
      "This module goes away in the next major version."
    ]
    #-}
  (post, total)
where

post :: Int -> [Int] -> [Int]
post = (:)

total :: [Int] -> Int
total = sum
