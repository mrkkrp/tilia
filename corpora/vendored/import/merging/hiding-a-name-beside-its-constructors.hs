module Studio.Palette where

import Studio.Colour hiding (Colour (Red), Colour, Colour (Blue), Shade, Shade (..))
import Studio.Colour (Colour, Colour (Green))
import Studio.Paint
  hiding (Paint (..), Paint)

data Swatch = Colour | Shade

swatch :: Swatch
swatch = Colour
