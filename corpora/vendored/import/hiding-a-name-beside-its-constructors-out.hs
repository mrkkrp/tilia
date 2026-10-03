module Studio.Palette where

import Studio.Colour (Colour (Green))
import Studio.Colour hiding (Colour, Colour (Blue, Red), Shade, Shade (..))
import Studio.Paint hiding (Paint, Paint (..))

data Swatch = Colour | Shade

swatch :: Swatch
swatch = Colour
