module Shape.Draw where

draw
  :: Double
  -- ^ How thick the lines are.

  -- Shapes are drawn in the order given.
  -> [Shape]
  -- ^ What to draw.
  -> IO ()
draw _ _ = pure ()

class Drawable a where
  render

    -- It is rendered at the origin.
    :: a
    -- ^ The thing to render.
    -> String
