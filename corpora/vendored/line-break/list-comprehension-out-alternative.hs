-- A list comprehension that runs past column 80 is broken before its bar,
-- one qualifier per line.
module Loom.Comprehensions where

spools looms = [spool | loom <- looms, spool <- loomSpools loom, spoolLength spool > minimumLength]
