data Connection = Connection
    { update
        :: Int
        -> IO ()
      -- not echoed back
    , close :: IO ()
    }

settings =
    Settings
        { retries = 3
          -- the server gives up after five
        , timeout = 10
        }

limits =
    [ sum
        [ 1
        , 2
        ]
      -- the two that are reserved
    , 3
    ]

convert
    :: Maybe Int
    -- the default goes first
    -> Int
convert = fromMaybe 0
