{-# LANGUAGE OverloadedStrings #-}

-- | Fixities for the few modules whose source defeats us.
module Tilia.Fixity.ByHand
  ( byHandFixities,
    hscFixities,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Tilia.Fixity

-- | The modules, and the operators they declare.
byHandFixities :: Map Text (Map OpName Fixity)
byHandFixities =
  Map.fromList
    [ -- @Test.QuickCheck.Property@ invokes a macro it defines:
      -- @WITNESSES(:: [Witness])@ stands in the middle of a record, and
      -- only @cpp@ can expand it. No configuration of the module is
      -- Haskell, so there is nothing to parse in any of them.
      entry
        "Test.QuickCheck.Property"
        [ ("==>", RightAssoc, 0),
          (".&.", RightAssoc, 1),
          (".&&.", RightAssoc, 1),
          (".||.", RightAssoc, 1),
          ("===", NoAssoc, 4),
          ("=/=", NoAssoc, 4)
        ]
    ]
  where
    entry name ops =
      (name, Map.fromList [(OpName o, Fixity d p) | (o, d, p) <- ops])

-- | What the modules written for @hsc2hs@ declare.
--
-- A different question from 'byHandFixities', and asked at a different
-- moment. That table is a last resort for a module nothing could be made
-- of; this one is the whole answer for an @.hsc@, given instead of reading
-- it, and a module absent from here declares nothing rather than being
-- unreadable.
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
