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
        ],
      -- @network@ writes its system calls as @foreign import CALLCONV@, and
      -- @CALLCONV@ is a macro out of @HsNetDef.h@ standing where the
      -- calling convention goes. It has to be expanded for the line to be
      -- Haskell at all, so no configuration of these five parses.
      entry "Network.Socket.If" [],
      entry "Network.Socket.Internal" [],
      entry "Network.Socket.Name" [],
      entry "Network.Socket.Shutdown" [],
      entry "Network.Socket.Syscall" [],
      -- @Data.HashMap.Internal.Array@ defines @CHECK_BOUNDS@ and calls it
      -- where an expression goes, with the guarded @case@ on the line
      -- below. Unexpanded it reads as a function applied to that @case@,
      -- and both branches of the @#if@ that defines it leave the call
      -- standing, so there is no configuration to fall back on.
      entry "Data.HashMap.Internal.Array" [],
      -- @monad-logger@ writes one method body once and hands it to sixteen
      -- instances: @#define DEF monadLoggerLog a b c d = …@, and then
      -- @instance … where DEF@ for each of them. A @where@ with a bare name
      -- after it is not Haskell, and the @#define@ sits outside every
      -- conditional, so there is no configuration in which it is.
      entry "Control.Monad.Logger" [],
      -- @cereal@ writes its generic sum instances through three macros, and
      -- the one that matters expands into a guard and its right-hand side
      -- at once: @gPut | PUTSUM(Word8) | …@. Unexpanded that is a guard
      -- with nothing after it.
      entry "Data.Serialize" [],
      -- @th-lift-instances@ has @LIFT_TYPED_DEFAULT@, defined three ways
      -- against the @template-haskell@ version and to nothing at all in the
      -- oldest of them, and written bare in five instance bodies.
      entry "Instances.TH.Lift" []
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
