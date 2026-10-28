{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | What GHC's own reader of flags makes of the flags of a header.
module Tilia.Pragma.Ghc
  ( InForce (..),
    inForceAfter,
  )
where

import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.EnumSet (EnumSet)
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

-- | What GHC puts in force, from its defaults, after the flags of a header.
data InForce = InForce
  { -- | The extensions on.
    inForceExtensions :: EnumSet Extension,
    -- | The edition named last, if one is.
    inForceEdition :: Maybe GHC.Language,
    -- | The Safe Haskell mode.
    inForceSafety :: SafeHaskellMode
  }

-- | What GHC's flag reader puts in force after these flags, read in order.
inForceAfter :: [Text] -> InForce
inForceAfter flags =
  InForce
    { inForceExtensions = GHC.extensionFlags dflags,
      inForceEdition = GHC.language dflags,
      inForceSafety = GHC.safeHaskell dflags
    }
  where
    dflags = snd (GHC.runCmdLineP reading defaults)
    reading = processArgs named (fmap noLoc args) (\_ -> pure [])
    named = mapMaybe ((`Map.lookup` switchingFlags) . drop 1) args
    args = fmap T.unpack flags

-- | The flags of GHC's that can switch an extension, the edition or the Safe
-- Haskell mode, by name: the @-X@ ones and those that spell an extension the
-- old way.
switchingFlags :: Map String (Flag (GHC.CmdLineP GHC.DynFlags))
switchingFlags =
  Map.fromList
    [(flagName f, f) | f <- GHC.flagsDynamic, switching (flagName f)]
  where
    switching name = "X" `isPrefixOf` name || name `elem` oldSpellings
    oldSpellings =
      ["cpp", "fglasgow-exts", "fno-glasgow-exts"]
        <> concat [["f" <> n, "fno-" <> n] | n <- GHC.flagSpecName <$> GHC.fLangFlags]

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
