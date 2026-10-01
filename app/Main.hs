{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Choice (Choice, fromBool, isTrue)
import Data.Foldable (traverse_)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
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
    stdin,
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
import Tilia.Editor (editorSession, formatBuffer)
import Tilia.Fixity.Debug (renderFixityNotes)
import Tilia.Format
  ( FormatError (Unreadable),
    PlanSource (..),
    Session,
    describeFormatError,
    fixityNotesOf,
    formatErrorExitCode,
    newSession,
  )
import Tilia.Palette (Color (Bad), Palette, paletteFor)
import Tilia.Parser (ghcLibParserVersion)
import Tilia.Run
  ( Outcome (..),
    Report (..),
    checkReport,
    differs,
    exitCodeOf,
    failIfDeclined,
    inplaceReport,
    noted,
    runOver,
    stdinReport,
    writeBack,
  )
import Tilia.Utils (asUtf8, lineWidth, quietly)

-- | The program's entry point.
main :: IO ()
main = do
  traverse_ transliterateUnprintable [stdout, stderr]
  opts <- customExecParser (prefs (columns lineWidth)) optsParserInfo
  palette <- paletteFor
  let cmd = optCommand opts
  outcomes <- case cmd of
    Inplace target -> do
      outcomes <- formatComponents palette opts target
      traverse_ writeBack outcomes
      outcomes <$ printReport (inplaceReport palette outcomes)
    Check target -> do
      outcomes <- formatComponents palette opts target
      outcomes <$ printReport (checkReport palette outcomes)
    ForEditor file -> do
      (input, outcomes) <- formatStdin palette opts file
      outcomes <$ printReport (stdinReport palette input outcomes)
  exitWith cmd outcomes

-- | Format the files of the components a target asks for.
formatComponents ::
  Palette ->
  Opts ->
  Maybe String ->
  IO [(FilePath, Outcome)]
formatComponents palette opts@Opts{..} target = do
  (root, components) <-
    componentsFor palette
      =<< either
        (die usageExitCode palette)
        pure
        (maybe (parseTarget "all") parseTarget target)
  files <-
    traverse makeRelativeToCurrentDirectory
      =<< filesOfComponents root components
  session <-
    newSession
      "."
      ( case optBuildPlan of
          Nothing -> PlanFromCabal (mapMaybe componentInPlan components)
          Just givenPlan -> GivenPlan givenPlan
      )
      optUseCache
      optDownload
      optCheckAst
      optCheckIdempotence
      optDebugFixity
      >>= either (dieFormatting palette) pure
  formatWith palette opts session (`runOver` files)

-- | Format the module read from standard input as the given file, and say
-- what became of it, together with the module as it was read.
formatStdin ::
  Palette ->
  Opts ->
  FilePath ->
  IO (Text, [(FilePath, Outcome)])
formatStdin palette opts@Opts{..} file = do
  input <- readStandardInput
  case asUtf8 input of
    Left why -> pure ("", [(file, Failed (Unreadable file why))])
    Right before -> do
      outcomes <-
        editorSession
          file
          optBuildPlan
          optUseCache
          optDownload
          optCheckAst
          optCheckIdempotence
          optDebugFixity
          >>= \case
            Left e -> pure [(file, Failed e)]
            Right session ->
              formatWith palette opts session $ \s ->
                pure . (file,) <$> formatBuffer s file before
      pure (before, outcomes)

-- | Format with a session and print how it settled fixities, counting a
-- declined file as failed where the options say so.
formatWith ::
  Palette ->
  Opts ->
  Session ->
  (Session -> IO [(FilePath, Outcome)]) ->
  IO [(FilePath, Outcome)]
formatWith palette Opts{..} session formatting = do
  outcomes <- formatting session
  printFixityNotes palette session
  pure $
    if isTrue optMustNotDecline
      then fmap (fmap failIfDeclined) outcomes
      else outcomes

-- | Read standard input to its end without closing it, which would hand its
-- descriptor to whatever is opened next.
readStandardInput :: IO BS.ByteString
readStandardInput = BS.concat <$> chunks
  where
    chunks = do
      chunk <- BS.hGetSome stdin 32768
      if BS.null chunk then pure [] else (chunk :) <$> chunks

-- | Print how the session settled every file's fixities, if it was asked to
-- keep an account of that.
printFixityNotes :: Palette -> Session -> IO ()
printFixityNotes palette session =
  fixityNotesOf session
    >>= traverse_ (T.hPutStrLn stderr) . renderFixityNotes palette

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

-- | Exit with a status code determined by the 'Command' and the formatting
-- 'Outcome's.
exitWith :: Command -> [(FilePath, Outcome)] -> IO ()
exitWith cmd outcomes = case exitCodeOf outcomes of
  Just code -> System.Exit.exitWith (ExitFailure code)
  Nothing -> case cmd of
    Inplace _ -> pure ()
    Check _ ->
      when
        (any (differs . snd) outcomes)
        (System.Exit.exitWith (ExitFailure 1))
    ForEditor _ -> pure ()

-- | Print a 'Report'.
printReport :: Report -> IO ()
printReport report = do
  BS.putStr (T.encodeUtf8 (reportOut report))
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

-- | Command.
data Command
  = -- | Format the files of a component, or of all of them, in place.
    Inplace (Maybe String)
  | -- | Report what formatting the files of a component, or of all of them,
    -- would change.
    Check (Maybe String)
  | -- | Format the module read from standard input as the given file.
    ForEditor FilePath

-- | The command line options.
data Opts = Opts
  { -- | What to do.
    optCommand :: Command,
    -- | Whether to check AST equivalence.
    optCheckAst :: Choice "checkAst",
    -- | Whether to check idempotence.
    optCheckIdempotence :: Choice "checkIdempotence",
    -- | Whether to print debugging information about fixities.
    optDebugFixity :: Choice "debugFixity",
    -- | A build plan to trust as up to date.
    optBuildPlan :: Maybe FilePath,
    -- | Whether to read from and write to the cache.
    optUseCache :: Choice "useCache",
    -- | Whether to download sources that are missing.
    optDownload :: Choice "download",
    -- | Whether to count declined files as failed.
    optMustNotDecline :: Choice "mustNotDecline"
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
        ( info
            (parser (Inplace <$> optional targetArgument))
            (progDesc "Format files, in place")
        ),
      command
        "check"
        ( info
            (parser (Check <$> optional targetArgument))
            (progDesc "Report what formatting would change, and fail if anything would")
        ),
      command
        "for-editor"
        ( info
            (parser (ForEditor <$> fileArgument))
            (progDesc "Format a module read from standard input as FILE, and print it")
        )
    ]
  where
    parser cmd =
      Opts
        <$> cmd
        <*> checkAstSwitch
        <*> checkIdempotenceSwitch
        <*> debugFixitySwitch
        <*> optional buildPlanOption
        <*> noCacheSwitch
        <*> noDownloadsSwitch
        <*> mustNotDeclineSwitch
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
    buildPlanOption =
      (strOption . mconcat)
        [ long "build-plan",
          metavar "PLAN",
          help "Trust this plan.json as up to date rather than have Cabal solve one"
        ]
    noCacheSwitch =
      fromBool . not
        <$> (switch . mconcat)
          [ long "no-cache",
            help "Neither read from nor write to the cache"
          ]
    noDownloadsSwitch =
      fromBool . not
        <$> (switch . mconcat)
          [ long "no-downloads",
            help "Do not download sources that are missing"
          ]
    mustNotDeclineSwitch =
      fromBool
        <$> (switch . mconcat)
          [ long "must-not-decline",
            help "Fail on files that would otherwise be declined"
          ]
    targetArgument =
      (strArgument . mconcat)
        [ metavar "COMPONENT",
          help "Component to format: all (the default) or a package/component name"
        ]
    fileArgument =
      (strArgument . mconcat)
        [ metavar "FILE",
          help "File whose contents are read from standard input"
        ]
