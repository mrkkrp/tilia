{-# LANGUAGE MultilineStrings #-}

-- Tabs after other indentation stop at the next multiple of eight, as GHC
-- reads them.
module Help.Options where

options :: String
options =
  """
  Options:
  	--batch      read recipes from a file
  		--strict  stop at the first warning
  	 	--quiet   print nothing
  """
