{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (when)
import Data.Choice (Choice, fromBool)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Version (showVersion)
import GHC.IO.Encoding (TextEncoding (textEncodingName))
import Options.Applicative
import Paths_tilia (version)
import System.Directory (makeRelativeToCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Exit qualified
import System.IO
  ( Handle,
    hFlush,
    hGetEncoding,
    hSetEncoding,
    mkTextEncoding,
    stderr,
    stdout,
  )
import Tilia.Cabal.Project (ProjectRoot, findProjectRoot)
import Tilia.Cabal.Target
  ( Component,
    Target,
    componentInPlan,
    componentsOfTarget,
    describeTargetProblem,
    filesOfComponents,
    parseTarget,
  )
import Tilia.Fixity.Debug (renderFixityNotes)
import Tilia.Format
  ( FormatError,
    describeFormatError,
    fixityNotesOf,
    formatErrorExitCode,
    newSession,
  )
import Tilia.Palette (Color (Bad), Palette, paletteFor)
import Tilia.Parser (ghcLibParserVersion)
import Tilia.Run
  ( Outcome,
    Report (..),
    checkReport,
    differs,
    exitCodeOf,
    inplaceReport,
    noted,
    runOver,
    writeBack,
  )
import Tilia.Utils (lineWidth, quietly)

-- | The program's entry point.
main :: IO ()
main = do
  traverse_ transliterateUnprintable [stdout, stderr]
  Opts {..} <- customExecParser (prefs (columns lineWidth)) optsParserInfo
  palette <- paletteFor
  target <-
    either
      (die usageExitCode palette)
      pure
      (maybe (parseTarget "all") parseTarget optTarget)
  (root, components) <- componentsFor palette target
  files <-
    traverse makeRelativeToCurrentDirectory
      =<< filesOfComponents root components
  session <-
    newSession
      "."
      (componentInPlan <$> components)
      optCheckAst
      optCheckIdempotence
      optDebugFixity
      >>= either (dieFormatting palette) pure
  outcomes <- runOver session files
  fixityNotesOf session
    >>= traverse_ (T.hPutStrLn stderr) . renderFixityNotes palette
  case optMode of
    Inplace -> do
      traverse_ writeBack outcomes
      printReport (inplaceReport palette outcomes)
    Check -> printReport (checkReport palette outcomes)
  exitWith optMode outcomes

-- | Transliterate unprintable characters if the output stream cannot handle
-- them.
transliterateUnprintable :: Handle -> IO ()
transliterateUnprintable h =
  quietly () $
    hGetEncoding h >>= \case
      Just encoding
        | name <- textEncodingName encoding,
          '/' `notElem` name ->
            hSetEncoding h =<< mkTextEncoding (name <> "//TRANSLIT")
      _ -> pure ()

-- | Exit with a status code determined by 'Mode' of operation and the
-- formatting 'Outcome's.
exitWith :: Mode -> [(FilePath, Outcome)] -> IO ()
exitWith mode outcomes = case exitCodeOf outcomes of
  Just code -> System.Exit.exitWith (ExitFailure code)
  Nothing -> case mode of
    Inplace -> pure ()
    Check ->
      when
        (any (differs . snd) outcomes)
        (System.Exit.exitWith (ExitFailure 1))

-- | Print a 'Report'.
printReport :: Report -> IO ()
printReport report = do
  traverse_ T.putStrLn (reportOut report)
  hFlush stdout
  traverse_ (T.hPutStrLn stderr) (reportErr report)
  hFlush stderr

-- | Every component the target asks for, and the project they were found
-- in, which is where the run's settings are read from as well.
componentsFor :: Palette -> Target -> IO (ProjectRoot, [Component])
componentsFor palette target =
  findProjectRoot "." >>= \case
    Nothing ->
      die 2 palette "no cabal.project or .cabal file at or above the working directory"
    Just root ->
      componentsOfTarget root target >>= \case
        Left problem ->
          die usageExitCode palette (describeTargetProblem problem)
        Right components ->
          pure (root, components)

-- | What @sysexits.h@ has called a usage error since 4.3BSD, and well clear
-- of the codes 'formatErrorExitCode' returns.
usageExitCode :: Int
usageExitCode = 64

-- | Give up, under the same mark a failed file wears.
die :: Int -> Palette -> Text -> IO a
die code palette why = do
  traverse_ (T.hPutStrLn stderr) (noted palette ("✗", Bad) why)
  System.Exit.exitWith (ExitFailure code)

-- | Print out the 'FormatError' and exit.
dieFormatting :: Palette -> FormatError -> IO a
dieFormatting palette e =
  die (formatErrorExitCode e) palette (describeFormatError palette e)

----------------------------------------------------------------------------
-- Command line options

-- | The mode of operation.
data Mode = Inplace | Check

-- | The command line options.
data Opts = Opts
  { -- | The mode of operation.
    optMode :: Mode,
    -- | Which component to work on, if not all of them.
    optTarget :: Maybe String,
    -- | Whether to check AST equivalence.
    optCheckAst :: Choice "checkAst",
    -- | Whether to check idempotence.
    optCheckIdempotence :: Choice "checkIdempotence",
    -- | Whether to print debugging information about fixities.
    optDebugFixity :: Choice "debugFixity"
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> versionOption <*> optsParser) . mconcat $
    [ fullDesc,
      progDesc "Format Haskell source code",
      header "tilia — a formatter for Haskell source code"
    ]
  where
    versionOption =
      infoOption
        ( "tilia "
            ++ showVersion version
            ++ "\nusing ghc-lib-parser "
            ++ ghcLibParserVersion
        )
        (long "version" <> short 'v' <> help "Print version of the program")

optsParser :: Parser Opts
optsParser =
  hsubparser . mconcat $
    [ command
        "inplace"
        (info (parser Inplace) (progDesc "Format files, in place")),
      command
        "check"
        (info (parser Check) (progDesc "Report what formatting would change, and fail if anything would"))
    ]
  where
    parser mode =
      Opts mode
        <$> optional targetArgument
        <*> checkAstSwitch
        <*> checkIdempotenceSwitch
        <*> debugFixitySwitch
    checkAstSwitch =
      fromBool
        <$> (switch . mconcat)
          [ long "check-ast",
            help "Check AST equivalence"
          ]
    checkIdempotenceSwitch =
      fromBool
        <$> (switch . mconcat)
          [ long "check-idempotence",
            help "Check idempotence"
          ]
    debugFixitySwitch =
      fromBool
        <$> (switch . mconcat)
          [ long "debug-fixity",
            help "Print debugging information about fixities"
          ]
    targetArgument =
      (strArgument . mconcat)
        [ metavar "COMPONENT",
          help "Component to format: all (the default) or a package/component name"
        ]
