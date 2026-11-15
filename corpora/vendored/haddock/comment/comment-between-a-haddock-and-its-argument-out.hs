module Shape.Draw where

draw ::
  -- | How thick the lines are.
  Double ->
  -- | What to draw.

  -- Shapes are drawn in the order given.
  [Shape] ->
  IO ()
draw _ _ = pure ()

class Drawable a where
  render ::
    -- | The thing to render.

    -- It is rendered at the origin.
    a ->
    String
