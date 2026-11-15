{-# LANGUAGE QuasiQuotes #-}

module Mail.Greeting where

greeting :: String
greeting = [str|Dear customer,|]

pending :: Query
pending = [sql| SELECT id FROM orders WHERE shipped = false |]
