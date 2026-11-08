module Shape.Draw where

-- Comments above an arrow, at the end of a line and between an argument's
-- Haddock and the next argument, and Haddocks under what they document.
draw ::
  -- What it draws with.
  Pen ->
  -- How thick the lines are.
  Double ->
  [Shape] -> -- in the order given
  IO ()
draw _ _ _ = pure ()

fill ::
  forall m.
  (MonadIO m) =>
  -- Only the outline is filled.

  -- | What to fill.
  Shape ->
  -- | What to fill it with.
  Colour ->

  -- Black, unless asked.

  -- | Done once the shape is filled.
  m ()
fill _ _ = pure ()

data Brush where
  Round ::
    -- | The radius.
    Double ->
    Brush
  Flat ::
    -- | The width.
    Double ->
    -- | The height.

    -- Flat brushes are square.
    Double ->
    Brush
