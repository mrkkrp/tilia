{-# LANGUAGE ImplicitParams #-}

module Report.Page where

paginate ::  (?pageSize
               :: Int
             ,?header :: String) => [String] -> [[String]]
paginate = chunk ?pageSize . (?header :)
