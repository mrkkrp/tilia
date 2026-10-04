-- A Haddock in a local signature documents nothing, so it comes out as
-- ordinary comments, a comment for each of its lines.
twoLines :: Int
twoLines = g 1 2
  where
    g
      :: Int -- ^ a
      -> Int -- ^ b, which goes
             --   on to a second line
      -> Int
    g = (+)

-- The same over three lines, the last of them lined up with the code.
threeLines :: Int
threeLines = g 1 2
  where
    g
      :: Int -- ^ a, which goes
             --   on to a second line
      --   and a third
      -> Int
      -> Int
    g = (+)

-- One on the lines above a local binding.
above :: Int
above = g
  where
    -- | What g is,
    -- over two lines.
    g = 1

-- A block one stays one comment.
block :: Int
block = g 1
  where
    g
      :: Int {- ^ a, which goes
                  on to a second line -}
      -> Int
    g = id
