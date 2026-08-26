{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatting a file, with everything the project can tell us about it.
module Tilia.Format
  ( FormatError (..),
    describeFormatError,
    formatErrorExitCode,
    formatFile,
  )
where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Tilia.Cpp (CppError (..), blankCpp, describeCppError, formatWithCpp, usesCpp)
import Tilia.Fixity.Plan (loadPlan, newResolver, scopeFor)
import Tilia.Parser
  ( ParseError,
    describeParseError,
    parseModule,
    parserConfigFor,
    pmModule,
  )
import Tilia.Pragma (effectiveExtensions, movesPositions)
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Package
  ( PackageProblem (..),
    PackageReader,
    describePackageProblem,
  )
import Tilia.Project (ProjectRoot (..), findProjectRoot)
import Tilia.Render (RenderConfig (..), defaultRenderConfig, renderModule)

-- | Why a file could not be formatted.
data FormatError
  = -- | No @cabal.project@, @stack.yaml@ or @.cabal@ file above it.
    NoProject FilePath
  | -- | A project, but no build plan we could read or produce. The text is
    -- whatever @cabal@ had to say about it.
    NoBuildPlan FilePath Text
  | -- | We failed to read .cabal file.
    NoPackage FilePath PackageProblem
  | -- | The file is not Haskell we can parse.
    NotParsed ParseError
  | -- | The file carries @{-# LINE #-}@ or @{-# COLUMN #-}@ pragmas.
    PositionPragmas FilePath
  | -- | The file uses the preprocessor in a way we cannot handle.
    CppUnsupported FilePath CppError

-- | Say what went wrong, in one line.
describeFormatError :: FormatError -> Text
describeFormatError = \case
  NoProject path ->
    "no project above " <> T.pack path <> ": expected a cabal.project, a stack.yaml or a .cabal file"
  NoBuildPlan root reason ->
    "no build plan for " <> T.pack root <> ": " <> reason
  NoPackage path problem ->
    "cannot tell what "
      <> T.pack path
      <> " is written in: "
      <> describePackageProblem problem
  NotParsed e -> "cannot parse " <> describeParseError e
  PositionPragmas path ->
    "will not format " <> T.pack path <> ": it uses {-# LINE #-} pragmas, and no reformatting can leave those true"
  CppUnsupported path why ->
    "will not format " <> T.pack path <> ": " <> describeCppError why

-- | The exit status a failure should leave behind.
formatErrorExitCode :: FormatError -> Int
formatErrorExitCode = \case
  NoProject {} -> 2
  NoBuildPlan {} -> 3
  NotParsed {} -> 4
  PositionPragmas {} -> 5
  NoPackage _ problem -> case problem of
    NoPackageFile -> 6
    PackageUnreadable {} -> 6
    PackageMalformed {} -> 7
    FileUnclaimed {} -> 8
  CppUnsupported _ why -> case why of
    UnhandledDirective {} -> 9
    UnsplittableConditional -> 10
    TooManyConfigurations -> 11
    ConfigurationNotParsed {} -> 12
    DirectiveUnplaceable {} -> 13
    DirectiveInQuotedText {} -> 14

-- | Format a file, using the project it belongs to.
formatFile ::
  -- | Package reader
  PackageReader ->
  -- | File to format
  FilePath ->
  -- | Result
  IO (Either FormatError Text)
formatFile askPackage path = runExceptT $ do
  source <- liftIO (T.readFile path)
  when (movesPositions source) $
    throwE (PositionPragmas path)
  root <- prPath <$> (need (NoProject path) =<< liftIO (findProjectRoot path))
  plan <- orElse (NoBuildPlan root) =<< liftIO (loadPlan root)
  package <- orElse (NoPackage path) =<< liftIO (askPackage path)
  resolve <- liftIO (newResolver plan)
  let extensionsInForce = effectiveExtensions package source
      config = parserConfigFor package
      extensions = Set.fromList extensionsInForce
      renderConfigFor hsModule = liftIO $ do
        scope <- scopeFor resolve hsModule
        pure defaultRenderConfig
          { rcExtensions = extensions,
            rcScope = Just scope
          }
  if usesCpp extensionsInForce source
    then do
      render <- case parseModule config path (blankCpp source) of
        Left _ -> pure defaultRenderConfig {rcExtensions = extensions}
        Right whole -> renderConfigFor (pmModule whole)
      orElse
        (CppUnsupported path)
        (formatWithCpp config render path source)
    else do
      parsed <- orElse NotParsed (parseModule config path source)
      render <- renderConfigFor (pmModule parsed)
      pure (printDoc defaultRenderOptions (renderModule render parsed))
  where
    need :: FormatError -> Maybe a -> ExceptT FormatError IO a
    need e = maybe (throwE e) pure
    orElse :: (e -> FormatError) -> Either e a -> ExceptT FormatError IO a
    orElse f = either (throwE . f) pure
