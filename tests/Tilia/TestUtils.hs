{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Helpers several specs share.
module Tilia.TestUtils
  ( withEnvironment,
    writeUtf8File,
    withFiles,
    withOpsProject,
    switchedOn,
    underAsciiLocale,
    parserConfigFor,
    defaultParserConfig,
    effectiveExtensionsFor,
    parsed,
    commentsIn,
    unreadOnly,
    inBothNamespaces,
    fingerprint,
    routedResolver,

    -- * Configurations, enumerated
    configurations,
    leaves,
    linearLeaves,
    countLeaves,
    answeredLeaves,
    answeredLinearLeaves,
  )
where

import Control.Exception (bracket)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Choice (pattern Do)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word (Word64)
import GHC.Driver.Session qualified as GHC
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding)
import GHC.LanguageExtensions.Type (Extension (..))
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO (mkTextEncoding)
import System.IO.Temp (withSystemTempDirectory)
import Tilia.Cabal.Package (newPackageReader)
import Tilia.Cabal.Plan (BuildPlan, plannedCompiler)
import Tilia.Comments (Comment)
import Tilia.Cpp.Directives
  ( Assignment,
    Configurations (..),
    CppError,
    GroupSpec (..),
    Guard,
    configurationsOn,
    gsCount,
    implied,
    nestedIn,
    readConditionals,
    resolved,
    scanConditionals,
    unconditionalErrors,
  )
import Tilia.Fixity (Fixities, Fixity, ModuleChain (..), Namespace (..), OpName)
import Tilia.Fixity.Plan (Resolver, Route, newResolverVia)
import Tilia.Parser
  ( ParsedModule (..),
    ParserConfig (..),
    describeParseError,
    parseModule,
  )
import Tilia.Pragma (Defaults, defaultsOf, effectiveExtensions)
import Tilia.Source (comments)

-- | Run something with the given variables set, and the environment as it
-- was afterwards however it turns out.
withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment vars act = bracket set restore (const act)
  where
    set = traverse remember vars
    remember (key, value) = do
      was <- lookupEnv key
      setEnv key value
      pure (key, was)
    restore = traverse_ (\(key, was) -> maybe (unsetEnv key) (setEnv key) was)

-- | Write a file as UTF-8, making the directories it is in.
writeUtf8File :: FilePath -> Text -> IO ()
writeUtf8File path text = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (T.encodeUtf8 text)

-- | Run something on a new temporary directory, by its canonical path,
-- holding these files at their paths in it.
withFiles :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withFiles files act =
  withSystemTempDirectory "tilia" $ \dir -> do
    root <- canonicalizePath dir
    traverse_ (\(path, text) -> writeUtf8File (root </> path) text) files
    act root

-- | Run something on a project of one package, @fake@, whose module @Ops@
-- declares @infixl 6 <+>@, and on a plan for it kept outside it that names
-- the project relatively, as @haskell.nix@ writes it.
withOpsProject ::
  -- | Further files of the project, by their paths in it, each @.hs@ file
  -- one more module of the package.
  [(FilePath, Text)] ->
  -- | Further entries of the plan.
  [Text] ->
  -- | Given the project's directory and the plan's path.
  (FilePath -> FilePath -> IO a) ->
  IO a
withOpsProject files others act =
  withFiles
    ( [ ("project" </> "fake.cabal", cabal),
        ("project" </> "src" </> "Ops.hs", ops),
        ("elsewhere" </> "plan.json", plan)
      ]
        <> [("project" </> path, text) | (path, text) <- files]
    )
    $ \dir -> act (dir </> "project") (dir </> "elsewhere" </> "plan.json")
  where
    cabal =
      T.unlines
        [ "cabal-version: 2.4",
          "name: fake",
          "version: 0.1.0.0",
          "library",
          "  exposed-modules: " <> T.intercalate ", " ("Ops" : modules),
          "  hs-source-dirs: src",
          "  default-language: Haskell2010"
        ]
    modules =
      [ T.pack (takeBaseName path)
      | (path, _) <- files,
        takeExtension path == ".hs"
      ]
    ops =
      T.unlines
        [ "module Ops ((<+>)) where",
          "infixl 6 <+>",
          "(<+>) :: a -> a -> a",
          "(<+>) = const"
        ]
    plan =
      "{\"compiler-id\":\"ghc-0.0\",\"install-plan\":["
        <> T.intercalate
          ","
          ( "{\"pkg-name\":\"fake\",\"pkg-version\":\"0.1.0.0\",\
            \\"pkg-src\":{\"type\":\"local\",\"path\":\"./.\"}}"
              : others
          )
        <> "]}"

-- | Is an environment variable set to anything but empty, @0@, @no@ or
-- @false@?
switchedOn :: String -> IO Bool
switchedOn name =
  maybe False (`notElem` ["", "0", "no", "false"]) <$> lookupEnv name

-- | Run something with files opened in ASCII, as under @LANG=C@, and the
-- locale's encoding as it was afterwards however it turns out.
underAsciiLocale :: IO a -> IO a
underAsciiLocale act = do
  ascii <- mkTextEncoding "ASCII"
  bracket getLocaleEncoding setLocaleEncoding $ \_ ->
    setLocaleEncoding ascii >> act

-- | What to parse a module with, given what its package puts in force, or
-- 'Nothing' for a module of no package, which is read as GHC2021.
parserConfigFor :: Maybe Defaults -> ParserConfig
parserConfigFor =
  ParserConfig . fromMaybe (defaultsOf (GHC.languageExtensions (Just GHC.GHC2021)) [])

-- | What to parse a module of no package with.
defaultParserConfig :: ParserConfig
defaultParserConfig = parserConfigFor Nothing

-- | The extensions in force in a module, given what its package puts in
-- force, or 'Nothing' for a module of no package, which starts from
-- @ImplicitPrelude@ alone.
--
-- That is less than 'parserConfigFor' reads such a module with, so that
-- what is printed, @import qualified@ for one, reads the same under every
-- edition.
effectiveExtensionsFor :: Maybe Defaults -> Text -> [Extension]
effectiveExtensionsFor = effectiveExtensions . fromMaybe (defaultsOf [ImplicitPrelude] [])

-- | Parse a module, or stop the test saying why it does not parse.
parsed :: Text -> ParsedModule
parsed source = case parseModule defaultParserConfig "M.hs" source of
  Left problem -> error (T.unpack (describeParseError problem))
  Right m -> m

-- | Every comment in a module.
commentsIn :: Text -> [Comment]
commentsIn = comments . pmSource . parsed

-- | The one import blamed for an operator, unread on its own account and so
-- with nothing below it.
unreadOnly :: Text -> NonEmpty ModuleChain
unreadOnly m = ModuleChain (m :| []) :| []

-- | Take fixities that say nothing about namespaces to govern both.
inBothNamespaces :: Map OpName Fixity -> Fixities
inBothNamespaces declared =
  Map.fromList
    [ ((namespace, op), fixity)
    | (op, fixity) <- Map.toList declared,
      namespace <- [InTypes, InTerms]
    ]

-- | FNV-1a, a hash of a string that is the same on every machine and every
-- run.
fingerprint :: String -> Word64
fingerprint = foldl' step 14695981039346656037
  where
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211

-- | A 'Resolver' that reads modules by the routes given and remembers what
-- it reads.
routedResolver :: [Route] -> BuildPlan -> IO Resolver
routedResolver routes plan = do
  packageReader <- newPackageReader (plannedCompiler plan) []
  newResolverVia (Do #useCache) routes packageReader plan

-- | Split a module on its first outermost conditional, and on every other
-- one written behind the same directives, wherever in the module it sits.
configurations :: Text -> Maybe Configurations
configurations source = do
  forest <- scanConditionals source
  gs <- listToMaybe forest
  pure (configurationsOn gs forest source)

-- | Every configuration of a module, with every conditional resolved.
leaves :: Text -> Either CppError [Text]
leaves = fmap (fmap snd) . answeredLeaves

-- | The configurations reached by varying one conditional at a time.
linearLeaves :: Text -> Either CppError [Text]
linearLeaves = fmap (fmap snd) . answeredLinearLeaves

-- | How many configurations a module has, without building any of them,
-- and so counting those no definition of the macros gives.
countLeaves :: Text -> Either CppError Integer
countLeaves source = do
  forest <- readConditionals source
  pure (sum [across answers forest | answers <- combinations (afforded forest)])
  where
    across answers = product . fmap (one answers)
    one answers gs = case lookup (gsGuards gs) answers of
      Just i -> across answers (nestedIn gs i)
      Nothing -> sum [across answers (nestedIn gs i) | i <- [0 .. gsCount gs - 1]]
    combinations = traverse (\(g, k) -> [(g, i) | i <- [0 .. k - 1]])
    afforded forest = go 1 (repeated forest)
      where
        go _ [] = []
        go n ((g, k) : rest)
          | n * toInteger k <= guardsToTie = (g, k) : go (n * toInteger k) rest
          | otherwise = []

-- | The guards a module asks more than once, and how many answers each has.
repeated :: [GroupSpec] -> [([Guard], Int)]
repeated forest = distinct Map.empty [q | q@(g, _) <- asked forest, twice g]
  where
    asked gss = concat [(gsGuards gs, gsCount gs) : asked (concat (gsNested gs)) | gs <- gss]
    times = Map.fromListWith (+) [(g, 1 :: Int) | (g, _) <- asked forest]
    twice g = Map.findWithDefault 0 g times >= 2

    distinct _ [] = []
    distinct seen (q@(g, _) : rest)
      | Map.member g seen = distinct seen rest
      | otherwise = q : distinct (Map.insert g () seen) rest

-- | How many combinations of answers 'countLeaves' will enumerate.
guardsToTie :: Integer
guardsToTie = 4096

-- | Every configuration some definition of the macros gives, and the
-- answers that reach it.
answeredLeaves :: Text -> Either CppError [(Assignment, Text)]
answeredLeaves =
  fmap (filter (isJust . implied . Map.toList . fst)) . go Map.empty
  where
    go answers source = case configurations source of
      Nothing -> answeredLeaf answers source
      Just c ->
        concat
          <$> traverse
            (\(i, t) -> go (Map.insert (cfgGuards c) i answers) t)
            (zip [0 ..] (cfgTexts c))

-- | The same, labelled by the answers that reach each one, and for the same
-- reason as 'answeredLeaves'.
answeredLinearLeaves :: Text -> Either CppError [(Assignment, Text)]
answeredLinearLeaves =
  fmap (filter (isJust . implied . Map.toList . fst)) . go Map.empty
  where
    go answers source = case configurations source of
      Nothing -> answeredLeaf answers source
      Just c -> case zip [0 ..] (cfgTexts c) of
        [] -> Right []
        (i, first) : rest ->
          (<>)
            <$> go (Map.insert (cfgGuards c) i answers) first
            <*> (concat <$> traverse (held answers (cfgGuards c)) rest)
      where
        held before gs (i, t) = answeredBaseline (Map.insert gs i before) t

-- | The configuration in which every question still to be asked takes its
-- first branch, and the answers that give it, unless it aborts with
-- @#error@.
answeredBaseline :: Assignment -> Text -> Either CppError [(Assignment, Text)]
answeredBaseline answers source = case configurations source of
  Nothing -> answeredLeaf answers source
  Just c -> case cfgTexts c of
    [] -> Right [(answers, source)]
    t : _ -> answeredBaseline (Map.insert (cfgGuards c) 0 answers) t

-- | A module with every conditional answered, and the answers that reach
-- it, unless it aborts with @#error@.
answeredLeaf :: Assignment -> Text -> Either CppError [(Assignment, Text)]
answeredLeaf answers source
  | null (unconditionalErrors source) = (\t -> [(answers, t)]) <$> resolved source
  | otherwise = Right []
