{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Formatting a file, with everything the project can tell us about it.
module Tilia.Format
  ( FormatError (..),
    describeFormatError,
    formatErrorExitCode,
    refused,
    Session (..),
    AstCheck (..),
    PlanSource (..),
    planSourceFor,
    newSession,
    Compiled (..),
    compiledSession,
    fixityNotesOf,
    formatSource,
    rewritten,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Choice (Choice, fromBool, isFalse, isTrue)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Version (Version, versionBranch)
import GHC.Driver.Session (OnOff (..))
import GHC.LanguageExtensions.Type (Extension (ImplicitPrelude))
import System.Directory (canonicalizePath)

import Tilia.Cabal.Package
  ( PackageProblem (..),
    PackageReader,
    compilerVersion,
    describePackageProblem,
    newPackageReader,
  )
import Tilia.Cabal.Plan
  ( PlanComponent,
    fetchUninstalled,
    loadPlan,
    macrosOf,
    plannedCompiler,
    readGivenPlan,
    unsolvedPlan,
    versionMacros,
  )
import Tilia.Cabal.Project (ProjectRoot (..), findProjectRoot)
import Tilia.Cabal.Target
  ( Component,
    TargetProblem,
    componentInPlan,
    describeTargetProblem,
    packageFilesOf,
    projectModules,
  )
import Tilia.Cpp
  ( CppError (..),
    answeredLeaves,
    answeredLinearLeaves,
    correspondingBranches,
    countLeaves,
    describeCppError,
    everyBranch,
    formatWithCpp,
    usesCpp,
    withoutRuledOut,
  )
import Tilia.Cpp.Macros (Macros)
import Tilia.Doc (printDoc)
import Tilia.Equivalence (commentDifference, syntaxDifference)
import Tilia.Fixity
  ( OpName,
    Unknown (..),
    operatorSpelling,
    spellDisagreement,
    spellUnreadIn,
    unknownOperators,
  )
import Tilia.Fixity.Debug (FixityNotes, fixityNotes)
import Tilia.Fixity.HiFile (HiFile)
import Tilia.Fixity.Plan
  ( Resolver (..),
    Route (FromInterface, FromSource),
    newResolverFrom,
    newResolverVia,
    scopeFor,
  )
import Tilia.MagicComments
  ( Misplacement (..),
    misplacedMagicComment,
    turnedOffThroughout,
  )
import Tilia.Palette (Color (Operator, Place), Palette, paint)
import Tilia.Parser
  ( ParseError,
    ParsedModule,
    ParserConfig (..),
    describeParseError,
    parseConfiguration,
    parseModule,
    pmGathered,
    pmModule,
  )
import Tilia.Pragma (defaultsOf, effectiveExtensions, movesPositions)
import Tilia.Render (RenderConfig (..), defaultRenderConfig, renderModule)
import Tilia.Source (Written (..), linesOf)
import Tilia.Utils (collected, spellList, tshow)

-- | Why a file could not be formatted.
data FormatError
  = -- | No @cabal.project@ or @.cabal@ file above it.
    NoProject FilePath
  | -- | A project, but no build plan we could read or produce, and why.
    NoBuildPlan FilePath Text
  | -- | A project whose own modules could not be told, and why.
    NoOwnModules FilePath TargetProblem
  | -- | The extensions in force in the file could not be settled, and why.
    NoPackage FilePath PackageProblem
  | -- | The file is not Haskell we can parse.
    NotParsed ParseError
  | -- | The file carries @{-# LINE #-}@ or @{-# COLUMN #-}@ pragmas, or lines
    -- GHC's lexer reads past, such as @#line@ ones.
    PositionPragmas FilePath
  | -- | The file uses the preprocessor in a way we cannot handle.
    CppUnsupported FilePath CppError
  | -- | A @{- TILIA_OFF -}@ or @{- TILIA_ON -}@ at this line cannot be
    -- honoured, and why.
    MisplacedMagicComment FilePath Int Misplacement
  | -- | An operator the file uses has a fixity we could not establish, as
    -- the file writes it.
    UnknownFixity FilePath [((Maybe Text, OpName), Unknown)]
  | -- | The file could not be read at all.
    Unreadable FilePath Text
  | -- | The file could not be written back once formatted.
    Unwritable FilePath Text
  | -- | Formatting the file changed its AST.
    NotEquivalent FilePath Text
  | -- | Formatting is not idempotent.
    NotIdempotent FilePath Text
  | -- | Formatting the file threw an exception, shown.
    Threw FilePath Text

-- | Say what went wrong.
describeFormatError :: Palette -> FormatError -> Text
describeFormatError palette = \case
  NoProject path ->
    "no project above "
      <> file path
      <> ": expected a cabal.project or a .cabal file"
  NoBuildPlan root reason ->
    "no build plan for " <> file root <> ": " <> reason
  NoOwnModules root problem ->
    "cannot tell which modules the project in "
      <> file root
      <> " declares: "
      <> describeTargetProblem problem
  NoPackage path problem ->
    "cannot tell what "
      <> file path
      <> " is written in: "
      <> describePackageProblem problem
  NotParsed e -> "cannot parse " <> located (describeParseError e)
  PositionPragmas path ->
    "will not format "
      <> file path
      <> ": it uses {-# LINE #-} or {-# COLUMN #-} pragmas or #line, #pragma"
      <> " or #! lines, and no reformatting can leave those true"
  CppUnsupported path why -> case why of
    ConfigurationNotParsed{} ->
      "will not format " <> located (describeCppError why)
    _ -> "will not format " <> file path <> ": " <> describeCppError why
  MisplacedMagicComment path n why ->
    "will not format "
      <> file path
      <> ": "
      <> case why of
        NotBetweenDeclarations ->
          "the {- TILIA_OFF -} or {- TILIA_ON -} at line "
            <> tshow n
            <> " is not on a line of its own between top-level declarations"
        AroundDirective ->
          "the region that {- TILIA_OFF -} starts at line "
            <> tshow n
            <> " includes a CPP directive which is not supported"
  Unreadable path why -> "cannot read " <> file path <> ": " <> why
  Unwritable path why -> "cannot write " <> file path <> ": " <> why
  NotEquivalent path why ->
    "formatting " <> file path <> " changed the program: " <> why
  NotIdempotent path why ->
    "formatting " <> file path <> " is not idempotent: " <> why
  Threw path why ->
    "formatting " <> file path <> " threw an exception: " <> why
  UnknownFixity path unknown ->
    "will not format "
      <> file path
      <> ": "
      <> T.intercalate ", and " (fmap saying (collected (fmap named unknown)))
    where
      named ((qualifier, op), why) =
        (why, paint palette Operator (operatorSpelling qualifier op))
      saying (why, ops) =
        (if single then "the fixity of " else "the fixities of ")
          <> spellList ops
          <> " "
          <> because
        where
          single = length ops == 1
          because = case why of
            NotRead missing ->
              "may be declared in " <> spellUnreadIn palette missing
            Ambiguous offers ->
              (if single then "is " else "are ")
                <> spellDisagreement palette offers
  where
    file = paint palette Place . T.pack
    located t = case T.breakOn ":" t of
      (where', rest) -> paint palette Place where' <> rest

-- | The exit status a failure should leave behind, distinct for each error
-- constructor.
formatErrorExitCode :: FormatError -> Int
formatErrorExitCode = \case
  NoProject{} -> 2
  NoBuildPlan{} -> 3
  NotParsed{} -> 4
  PositionPragmas{} -> 5
  NoPackage _ problem -> case problem of
    NoPackageFile -> 6
    PackageUnreadable{} -> 7
    PackageMalformed{} -> 8
    FileUnclaimed{} -> 9
  CppUnsupported _ why -> case why of
    MalformedConditional{} -> 10
    TooManyConfigurations -> 11
    ConfigurationNotParsed{} -> 12
    DirectiveInQuotedText{} -> 13
    MacroInQuotedText{} -> 14
    BeforeLeadingComma{} -> 15
    RuledOutBranch{} -> 16
    AbortingAlternative{} -> 17
  UnknownFixity{} -> 18
  Unreadable{} -> 19
  NotEquivalent{} -> 20
  NotIdempotent{} -> 21
  Threw{} -> 22
  Unwritable{} -> 23
  MisplacedMagicComment{} -> 24
  NoOwnModules{} -> 25

-- | Did we decline to format the file, rather than fail to?
refused :: FormatError -> Bool
refused = \case
  PositionPragmas{} -> True
  CppUnsupported{} -> True
  MisplacedMagicComment{} -> True
  UnknownFixity{} -> True
  NotParsed{} -> False
  NoPackage{} -> False
  Unreadable{} -> False
  Unwritable{} -> False
  NotEquivalent{} -> False
  NotIdempotent{} -> False
  Threw{} -> False
  NoProject{} -> False
  NoBuildPlan{} -> False
  NoOwnModules{} -> False

-- | What a run works out once and then uses for every file.
data Session = Session
  { -- | The project.
    sessionRoot :: ProjectRoot,
    -- | The modules the project's own packages declare, each with the names
    -- of those that declare it.
    sessionOwnModules :: Map Text (Set Text),
    -- | What can be asked about the modules a file imports.
    sessionResolver :: Resolver,
    -- | What the plan settles about the questions a file's conditionals
    -- ask, so that a branch it rules out is not read as part of the file.
    sessionMacros :: Macros,
    -- | What each file's package puts in force.
    sessionPackage :: PackageReader,
    -- | Whether to check AST equivalence, and in which configurations.
    sessionCheckAst :: AstCheck,
    -- | Whether to check idempotence.
    sessionCheckIdempotence :: Choice "checkIdempotence",
    -- | An account of how each file's fixities were determined, or 'Nothing'
    -- where none was asked for.
    sessionFixityNotes :: Maybe (IORef (Map FilePath FixityNotes)),
    -- | Why Cabal could not solve a plan for the project, where the session
    -- went on without one.
    sessionUnsolved :: Maybe Text
  }

-- | Whether to check AST equivalence, and in which configurations of a
-- module that uses the preprocessor.
data AstCheck
  = -- | Not at all.
    NoAstCheck
  | -- | In one configuration for each branch of each conditional, as a run
    -- does.
    EachCppBranch
  | -- | In every configuration, up to as many as given, and past that in
    -- the ones varying a single conditional, once both sides have as many.
    EachCppConfiguration Int
  deriving (Eq, Show)

-- | The AST check a run makes where it is asked for one.
sessionProdAstCheck ::
  -- | Whether to check AST equivalence.
  Choice "checkAst" ->
  AstCheck
sessionProdAstCheck checkAst =
  if isTrue checkAst then EachCppBranch else NoAstCheck

-- | Where a session's build plan comes from.
data PlanSource
  = -- | The project's own, solved again or fetched for with Cabal.
    PlanFromCabal [PlanComponent]
  | -- | A plan file trusted as up to date, so that we can avoid running
    -- Cabal to solve it.
    GivenPlan FilePath

-- | Where a session's build plan comes from: the plan file given, if any,
-- or else Cabal, solving for the components the action finds.
planSourceFor :: Maybe FilePath -> IO [Component] -> IO PlanSource
planSourceFor givenPlan components = case givenPlan of
  Just plan -> pure (GivenPlan plan)
  Nothing -> PlanFromCabal . mapMaybe componentInPlan <$> components

-- | Settle everything that does not depend on the file being formatted,
-- with only the project's own packages and the compiler's where Cabal
-- cannot solve a plan.
newSession ::
  -- | The project.
  ProjectRoot ->
  -- | Where the build plan comes from.
  PlanSource ->
  -- | Whether to read from and write to the cache.
  Choice "useCache" ->
  -- | Whether to download sources that are missing.
  Choice "download" ->
  -- | Check AST equivalence.
  Choice "checkAst" ->
  -- | Check idempotence.
  Choice "checkIdempotence" ->
  -- | Record how every file's fixities were settled, to be read afterwards
  -- with 'fixityNotesOf'.
  Choice "debugFixity" ->
  IO (Either FormatError Session)
newSession project planSource caching downloading checkAst checkIdempotence debugFixity = runExceptT $ do
  let root = prPath project
  packages <- orElse (NoOwnModules root) =<< liftIO (packageFilesOf project)
  ownModules <- orElse (NoOwnModules root) =<< liftIO (projectModules packages)
  (plan, routes, unsolved) <- case planSource of
    PlanFromCabal components ->
      liftIO (loadPlan caching downloading components root) >>= \case
        Right plan -> pure (plan, [FromInterface, FromSource], Nothing)
        Left why -> do
          plan <- liftIO ((`unsolvedPlan` packages) =<< compilerVersion)
          pure (plan, [FromInterface], Just why)
    GivenPlan path -> do
      plan <- orElse (NoBuildPlan root) =<< liftIO (readGivenPlan root path)
      routes <-
        if isTrue downloading
          then
            [FromInterface, FromSource]
              <$ liftIO (fetchUninstalled caching root plan)
          else pure [FromInterface]
      pure (plan, routes, Nothing)
  packageReader <- liftIO (newPackageReader (plannedCompiler plan) packages)
  resolver <- liftIO (newResolverVia caching routes packageReader root plan)
  notes <-
    if isTrue debugFixity
      then Just <$> liftIO (newIORef Map.empty)
      else pure Nothing
  pure
    Session
      { sessionRoot = project,
        sessionOwnModules = ownModules,
        sessionResolver = resolver,
        sessionMacros = macrosOf plan,
        sessionPackage = packageReader,
        sessionCheckAst = sessionProdAstCheck checkAst,
        sessionCheckIdempotence = checkIdempotence,
        sessionFixityNotes = notes,
        sessionUnsolved = unsolved
      }

-- | What compiling a file's component establishes, which a session
-- otherwise works out with Cabal, the package database, and the sources of
-- dependencies.
data Compiled = Compiled
  { -- | The extensions the component puts in force, which an edition a
    -- module names adds to, as which of them its own edition brought is not
    -- known.
    compiledExtensions :: [Extension],
    -- | The version of the compiler.
    compiledWith :: Version,
    -- | The version of each package the component depends on, by name.
    compiledPackages :: Map Text Version,
    -- | The interface of a module the component can import, by name.
    compiledInterface :: Text -> IO (Maybe HiFile)
  }

-- | Settle everything that does not depend on the file being formatted from
-- what compiling its component establishes, and from the project's @.cabal@
-- files which of its modules are its own.
compiledSession ::
  -- | The file.
  FilePath ->
  -- | What compiling its component establishes.
  Compiled ->
  -- | Check AST equivalence.
  Choice "checkAst" ->
  -- | Check idempotence.
  Choice "checkIdempotence" ->
  IO (Either FormatError Session)
compiledSession file Compiled{..} checkAst checkIdempotence = runExceptT $ do
  project <-
    maybe (throwE (NoProject file)) pure =<< liftIO (findProjectRoot file)
  let root = prPath project
  packages <- orElse (NoOwnModules root) =<< liftIO (packageFilesOf project)
  ownModules <- orElse (NoOwnModules root) =<< liftIO (projectModules packages)
  resolver <- liftIO (newResolverFrom compiledInterface)
  pure
    Session
      { sessionRoot = project,
        sessionOwnModules = ownModules,
        sessionResolver = resolver,
        sessionMacros =
          versionMacros
            (Just (numbers compiledWith))
            (fmap numbers compiledPackages),
        sessionPackage =
          \_ -> pure (Right (defaultsOf [] (On <$> compiledExtensions))),
        sessionCheckAst = sessionProdAstCheck checkAst,
        sessionCheckIdempotence = checkIdempotence,
        sessionFixityNotes = Nothing,
        sessionUnsolved = Nothing
      }
  where
    numbers = fmap toInteger . versionBranch

-- | What the run made of every file's operators, by file, empty unless the
-- session keeps an account of it.
fixityNotesOf :: Session -> IO (Map FilePath FixityNotes)
fixityNotesOf session = case sessionFixityNotes session of
  Nothing -> pure Map.empty
  Just ref -> readIORef ref

-- | Format source that has already been read.
formatSource ::
  -- | What the run has worked out already.
  Session ->
  -- | The file the source came from.
  FilePath ->
  -- | The source.
  Text ->
  -- | Result.
  IO (Either FormatError Text)
formatSource session path source
  | turnedOffThroughout source = pure (Right source)
  | otherwise = formatTurnedOn session path source

-- | 'formatSource' for a module no @{- TILIA_OFF -}@ turns off throughout.
formatTurnedOn ::
  -- | What the run has worked out already.
  Session ->
  -- | The file the source came from.
  FilePath ->
  -- | The source.
  Text ->
  IO (Either FormatError Text)
formatTurnedOn session path source = runExceptT $ do
  package <- orElse (NoPackage path) =<< liftIO (sessionPackage session path)
  importer <- liftIO (canonicalizePath path)
  let resolver = sessionResolver session
      config = ParserConfig package
      misplaced (n, why) = throwE (MisplacedMagicComment path n why)
      extensionsAndCpp text =
        let declared = effectiveExtensions package text
         in (Set.fromList declared, usesCpp declared text)
      renderConfigWith extensions =
        defaultRenderConfig
          { rcOwnModules = sessionOwnModules session,
            rcExtensions = extensions
          }
      renderConfigFor extensions parsed = do
        let implicitPrelude =
              fromBool (Set.member ImplicitPrelude extensions)
        scope <-
          liftIO $
            scopeFor
              resolver
              (Just importer)
              implicitPrelude
              (fmap pmModule parsed)
        liftIO $ case sessionFixityNotes session of
          Nothing -> pure ()
          Just ref -> do
            told <-
              fixityNotes
                implicitPrelude
                (askModule resolver (Just importer))
                scope
                parsed
            atomicModifyIORef'
              ref
              (\m -> (Map.insertWith (\_ old -> old) path told m, ()))
        case unknownOperators scope (fmap pmGathered parsed) of
          [] -> pure (renderConfigWith extensions){rcScope = Just scope}
          unknown -> throwE (UnknownFixity path unknown)
      formatting (extensions, cpp) already text
        | isTrue cpp = do
            render <- case everyBranch
              config
              path
              (withoutRuledOut (sessionMacros session) text) of
              Nothing -> pure (renderConfigWith extensions)
              Just branches -> do
                traverse_ misplaced (misplacedMagicComment text branches)
                renderConfigFor extensions branches
            printed <-
              orElse
                (CppUnsupported path)
                (formatWithCpp config render path text)
            pure (printed, Nothing)
        | otherwise = do
            parsed <-
              maybe
                (orElse NotParsed (parseModule config path text))
                pure
                already
            traverse_ misplaced (misplacedMagicComment text (pure parsed))
            render <- renderConfigFor extensions (pure parsed)
            pure
              ( printDoc (renderModule render parsed),
                Just parsed
              )
  let inForce@(extensions, cpp) = extensionsAndCpp source
  when (movesPositions (Set.toList extensions) source) $
    throwE (PositionPragmas path)
  (formatted, tree) <- formatting inForce Nothing source
  let (changed, printedTree) =
        rewritten config astCheck cpp path (source, tree) formatted
      astCheck = sessionCheckAst session
  traverse_ (throwE . NotEquivalent path) changed
  when (isTrue (sessionCheckIdempotence session)) $ do
    (settled, _) <-
      formatting (extensionsAndCpp formatted) printedTree formatted
    when (settled /= formatted) $
      throwE (NotIdempotent path (whyNotIdempotent formatted settled))
  pure formatted

-- | Why a second formatting pass did not come out like the first.
whyNotIdempotent ::
  -- | What the first pass printed.
  Text ->
  -- | What the second pass printed.
  Text ->
  Text
whyNotIdempotent before after =
  case [n | (n, one, two) <- zip3 [1 :: Int ..] first second, one /= two] of
    (n : _) -> "line " <> tshow n <> " differs"
    [] ->
      "the second pass came out "
        <> tshow (length second)
        <> " lines long where the first came out "
        <> tshow (length first)
  where
    first = T.lines before
    second = T.lines after

-- | What formatting changed about the program.
rewritten ::
  -- | How to parse both sides.
  ParserConfig ->
  -- | Whether to compare the two, and in which configurations where the
  -- file uses the preprocessor.
  AstCheck ->
  -- | Whether the file uses the preprocessor.
  Choice "cppUsed" ->
  -- | The file, for the parser's messages.
  FilePath ->
  -- | What was read, and the tree it was printed from where it has one.
  (Text, Maybe ParsedModule) ->
  -- | What was printed.
  Text ->
  -- | What formatting changed, and the tree of what was printed.
  (Maybe Text, Maybe ParsedModule)
rewritten config astCheck cppUsed path (before, printedFrom') after
  | NoAstCheck <- astCheck = (Nothing, Nothing)
  | isFalse cppUsed = case parseModule config path after of
      Left e ->
        ( Just ("the formatted output does not parse: " <> describeParseError e),
          Nothing
        )
      Right a' -> case printedFrom'
        <|> whatParsed (parseModule config path before) of
        Nothing -> (Nothing, Just a')
        Just b' -> (comparing b' a', Just a')
  | otherwise = (underCpp, Nothing)
  where
    comparing b' a' =
      syntaxDifference (pmModule b') (pmModule a')
        <|> commentDifference b' a'
    whatParsed = either (const Nothing) Just
    underCpp = case astCheck of
      NoAstCheck -> Nothing
      EachCppBranch -> case correspondingBranches before after of
        Left _ ->
          Just "the input or output could not be split into configurations"
        Right pairs -> firstJust [compareBranch b a | (b, a) <- pairs]
      EachCppConfiguration limit ->
        case (countLeaves before, countLeaves after) of
          (Left why, _) -> unsplit "input" why
          (_, Left why) -> unsplit "output" why
          (Right went, Right came)
            | went /= came ->
                Just
                  ( "formatting changed how many configurations there are,"
                      <> " from "
                      <> tshow went
                      <> " to "
                      <> tshow came
                  )
            | otherwise -> case (enumerate before, enumerate after) of
                (Left why, _) -> unsplit "input" why
                (_, Left why) -> unsplit "output" why
                (Right b, Right a) -> alongside b a
            where
              enumerate
                | went <= toInteger limit = answeredLeaves
                | otherwise = answeredLinearLeaves
    unsplit side why =
      Just ("the " <> side <> "'s configurations: " <> describeCppError why)
    alongside went came =
      firstJust $
        [ maybe
            (Just "a configuration of the input the output does not have")
            (compareBranch (Just b) . Just)
            (Map.lookup answers output)
        | (answers, b) <- went
        ]
          <> [ Just "a configuration of the output the input does not have"
             | any ((`Map.notMember` input) . fst) came
             ]
      where
        input = Map.fromList went
        output = Map.fromList came
    compareBranch Nothing Nothing = Nothing
    compareBranch Nothing (Just _) =
      Just "a configuration no longer aborts preprocessing"
    compareBranch (Just _) Nothing =
      Just "a configuration now aborts preprocessing"
    compareBranch (Just b) (Just a) =
      ("in one configuration, " <>) <$> difference b a
    difference b a = case (configuration before b, configuration after a) of
      (Left e, _) -> unread e
      (_, Left e) ->
        Just ("the formatted output does not parse: " <> describeParseError e)
      (Right b', Right a') -> comparing b' a'
    configuration whole =
      parseConfiguration config path (linesOf (Written whole))
    unread e = case astCheck of
      EachCppConfiguration _ ->
        Just ("the input does not parse: " <> describeParseError e)
      _ -> Nothing
    firstJust = foldr (<|>) Nothing

-- | Give up with the given error where there is one to give up over.
orElse :: (e -> FormatError) -> Either e a -> ExceptT FormatError IO a
orElse f = either (throwE . f) pure
