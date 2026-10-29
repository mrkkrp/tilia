{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | What GHC's own reader of flags makes of the flags of a header.
module Tilia.Pragma.Ghc
  ( InForce (..),
    inForceAfter,
    readFlags,
  )
where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Driver.CmdLine (Flag (..), processArgs)
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension)
import GHC.Platform (genericPlatform)
import GHC.Settings
import GHC.Settings.Config (cProjectVersion)
import GHC.Types.SafeHaskell (SafeHaskellMode)
import GHC.Types.SrcLoc (noLoc)
import GHC.Unit.Types (stringToUnitId)
import GHC.Utils.Fingerprint (fingerprint0)
import Tilia.Pragma (flagSwitches)

-- | What the flags of a header settle, whatever was in force before them.
data InForce = InForce
  { -- | Whether the flags, with what they imply, last turn each extension
    -- they switch on or off.
    inForceSwitches :: Map Extension Bool,
    -- | The edition named last, if one is.
    inForceEdition :: Maybe GHC.Language,
    -- | The Safe Haskell mode.
    inForceSafety :: SafeHaskellMode
  }

-- | What GHC's flag reader makes of these flags, read in order.
inForceAfter :: [Text] -> InForce
inForceAfter flags =
  InForce
    { inForceSwitches = Map.fromList (fmap switched (reverse (GHC.extensions dflags))),
      inForceEdition = GHC.language dflags,
      inForceSafety = GHC.safeHaskell dflags
    }
  where
    dflags = readFlags flags
    switched = \case
      GHC.On e -> (e, True)
      GHC.Off e -> (e, False)

-- | What GHC's flag reader makes of these flags, read in order from its
-- defaults, of the flags that can switch an extension, the edition or the
-- Safe Haskell mode.
readFlags :: [Text] -> GHC.DynFlags
readFlags flags = snd (GHC.runCmdLineP reading defaults)
  where
    reading = processArgs named (fmap noLoc args) (\_ -> pure [])
    named = mapMaybe ((`Map.lookup` switchingFlags) . drop 1) args
    args = fmap T.unpack flags

-- | The flags of GHC's that can switch an extension, the edition or the Safe
-- Haskell mode, by name: the @-X@ ones and the rest of those 'flagSwitches'
-- reads.
switchingFlags :: Map String (Flag (GHC.CmdLineP GHC.DynFlags))
switchingFlags =
  Map.fromList
    [(flagName f, f) | f <- GHC.flagsDynamic, switching (flagName f)]
  where
    switching name =
      "X" `isPrefixOf` name || not (null (flagSwitches (T.pack ('-' : name))))

-- | What GHC puts in force before it reads any flag.
defaults :: GHC.DynFlags
defaults = GHC.defaultDynFlags settings

-- | Enough of a compiler's settings for its flag reader, which runs no tool.
settings :: Settings
settings =
  Settings
    { sGhcNameVersion = GhcNameVersion "ghc" cProjectVersion,
      sFileSettings = FileSettings{},
      sTargetPlatform = genericPlatform,
      sToolSettings = ToolSettings{toolSettings_opt_P_fingerprint = fingerprint0},
      sPlatformMisc = PlatformMisc{},
      sUnitSettings = UnitSettings (stringToUnitId "base"),
      sRawSettings = []
    }
