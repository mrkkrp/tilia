{-# LANGUAGE MultilineStrings #-}

-- Tabs that indent the lines of a multiline string come out as the spaces
-- they stand for, which is how GHC reads them.
module Help.Commands where

commands :: String
commands =
  """
  Commands:
  	build   compile the recipe
  	  --fast  skip the checks
  	test    run the checks
  """
