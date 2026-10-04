{-# LANGUAGE OverloadedStrings #-}

-- | Fixities for the modules written for @hsc2hs@, which are not read.
module Tilia.Fixity.ByHand
  ( hscFixities,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Tilia.Fixity

-- | What the modules written for @hsc2hs@ declare.
--
-- This is the whole answer for an @.hsc@, given instead of reading it, and
-- a module absent from here declares nothing rather than being unreadable.
hscFixities :: Map Text (Map OpName Fixity)
hscFixities =
  Map.fromList
    [ -- @addSignal@ and @deleteSignal@ take a signal on the left and a set
      -- on the right, so a chain of them only typechecks to the right, and
      -- the module says so with a bare @infixr@—precedence 9, as the Report
      -- has it when none is written.
      entry
        "System.Posix.Signals"
        [ ("addSignal", RightAssoc, 9),
          ("deleteSignal", RightAssoc, 9)
        ]
    ]
  where
    entry name ops =
      (name, Map.fromList [(OpName o, Fixity d p) | (o, d, p) <- ops])
