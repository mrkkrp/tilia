module Shape.Draw where

-- Comments above an arrow, at the end of a line and between an argument's
-- Haddock and the next argument, and Haddocks under what they document.
draw
  -- What it draws with.
  :: Pen
  -- How thick the lines are.
  -> Double
  -> [Shape] -- in the order given
  -> IO ()
draw _ _ _ = pure ()

fill
  :: forall m
   . (MonadIO m)
  -- Only the outline is filled.
  => Shape
  -- ^ What to fill.
  -> Colour
  -- ^ What to fill it with.

  -- Black, unless asked.
  -> m ()
  -- ^ Done once the shape is filled.
fill _ _ = pure ()

data Brush where
  Round
    :: Double
    -- ^ The radius.
    -> Brush
  Flat
    :: Double
    -- ^ The width.

    -- Flat brushes are square.
    -> Double
    -- ^ The height.
    -> Brush
