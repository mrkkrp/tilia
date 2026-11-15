module Shape.Kind where

data Shape
  = Circle Double
  -- | A rectangle, given its width and height.

  -- Squares are rectangles too, for now.
  | Rectangle Double Double
  -- | A polygon, given its corners,
  -- in clockwise order.

  -- Corners may repeat.
  | Polygon [(Double, Double)]

data Style = Style
  { strokeWidth :: Double
  -- | The colour of the fill.

  -- Transparent when absent.
  , fill :: Maybe String
  }
