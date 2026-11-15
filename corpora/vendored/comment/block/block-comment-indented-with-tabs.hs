-- The lines of a moved block comment stay where they were against its first
-- line, a tab counting up to the next multiple of eight, as GHC counts.
module Tabs where

underTabs :: Int
underTabs = g
	where
		{- a
                   b -}
		g = 1

overATab :: Int
overATab = g
    where
      {- a
	  b -}
      g = 1
