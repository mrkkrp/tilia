oneElement =
  [ first
    -- and nothing after it
  ]

severalElements =
  [ first,
    second
    -- and nothing after them
  ]

blockComment =
  [ first
    {- room
       for more -}
  ]

nested =
  [ outer,
    [ inner
      -- innermost
    ]
  ]
