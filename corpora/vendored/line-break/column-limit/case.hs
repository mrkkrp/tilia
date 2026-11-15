-- A case written on one line that runs past column 80 is broken, one
-- alternative per line.
module Loom.Cases where

shade colour = case colour of Dyed dye -> saturationOf dye; Bleached -> 0; Plain -> defaultShade
