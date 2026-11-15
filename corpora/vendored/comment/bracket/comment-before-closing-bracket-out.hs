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

comprehension =
  [ x | x <- xs
  -- and only those
  ]
    ++ rest

comprehensionAlone =
  [ x | x <- xs
  -- and nothing after it
  ]

openSequence =
  [ first
    ..
    -- and on from there
  ]
    ++ rest
