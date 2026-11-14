{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Helpers several specs share.
module Tilia.TestUtils
  ( withEnvironment,
    withFiles,
    withOpsProject,
    switchedOn,
    underAsciiLocale,
    orthogonalityMode,
    testedSettings,
    withEachSettings,
    defaultParserConfig,
    parsed,
    parses,
    commentsIn,
    haskellFilesIn,
    unreadOnly,
    inBothNamespaces,
    fingerprint,
    routedResolver,

    -- * Configurations, enumerated
    leaves,
    linearLeaves,
  )
where

import Control.Exception (bracket)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Choice (pattern Do)
import Data.Either (isRight)
import Data.Foldable (for_, traverse_)
import Data.List (isSuffixOf, sort)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Driver.Session qualified as GHC
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding)
import System.Directory
  ( canonicalizePath,
    createDirectoryIfMissing,
    doesDirectoryExist,
    listDirectory,
  )
import System.Environment (lookupEnv)
import System.Environment.Blank (setEnv, unsetEnv)
import System.FilePath (takeBaseName, takeDirectory, takeExtension, (</>))
import System.IO (mkTextEncoding)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, runIO)

import Tilia.Cabal.Package (newPackageReader, sourceExtensions)
import Tilia.Cabal.Plan (BuildPlan, plannedCompiler, plannedFlags)
import Tilia.Comments (Comment)
import Tilia.Cpp.Directives (CppError, answeredLeaves, answeredLinearLeaves)
import Tilia.Fixity (Fixities, Fixity, ModuleChain (..), Namespace (..), OpName)
import Tilia.Fixity.Plan (Resolver, Route, newResolverVia)
import Tilia.Parser
  ( ParsedModule (..),
    ParserConfig (..),
    describeParseError,
    parseModule,
  )
import Tilia.Pragma (defaultsOf)
import Tilia.Settings (Settings (..), alternativeSettings, defaultSettings)
import Tilia.Source (comments)
import Tilia.Utils (quietly, writeUtf8File)

-- | Run something with the given variables set, to empty values too, and the
-- environment as it was afterwards however it turns out.
withEnvironment :: [(String, String)] -> IO a -> IO a
withEnvironment vars act = bracket set restore (const act)
  where
    set = traverse remember vars
    remember (key, value) = do
      was <- lookupEnv key
      setEnv key value True
      pure (key, was)
    restore = traverse_ $ \(key, was) ->
      maybe (unsetEnv key) (\value -> setEnv key value True) was

-- | Run something on a new temporary directory, by its canonical path,
-- holding these files at their paths in it.
withFiles :: [(FilePath, Text)] -> (FilePath -> IO a) -> IO a
withFiles files act =
  withSystemTempDirectory "tilia" $ \dir -> do
    root <- canonicalizePath dir
    for_ files $ \(path, text) -> do
      createDirectoryIfMissing True (takeDirectory (root </> path))
      writeUtf8File (root </> path) text
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

-- | Is the suite to try every combination of the settings, as
-- @TILIA_ORTHOGONALITY@ asks?
orthogonalityMode :: IO Bool
orthogonalityMode = switchedOn "TILIA_ORTHOGONALITY"

-- | The settings the suite formats with, each with its name:
-- 'defaultSettings' and 'alternativeSettings', or every combination of the
-- settings but the column limit in the 'orthogonalityMode'.
testedSettings :: IO [(String, Settings)]
testedSettings = do
  orthogonality <- orthogonalityMode
  pure $
    if orthogonality
      then
        [ ( show commas <> ", " <> show arrows,
            defaultSettings
              { settingsCommaStyle = commas,
                settingsArrowStyle = arrows
              }
          )
        | commas <- [minBound .. maxBound],
          arrows <- [minBound .. maxBound]
        ]
      else
        [ ("the default settings", defaultSettings),
          ("the alternative settings", alternativeSettings)
        ]

-- | Run specs once with each of the 'testedSettings'.
withEachSettings ::
  -- | The specs, given the settings.
  (Settings -> Spec) ->
  Spec
withEachSettings specs = do
  tested <- runIO testedSettings
  for_ tested $ \(name, settings) -> describe ("with " <> name) (specs settings)

-- | What to parse a module of no package with: GHC2021.
defaultParserConfig :: ParserConfig
defaultParserConfig =
  ParserConfig (defaultsOf (GHC.languageExtensions (Just GHC.GHC2021)) [])

-- | Parse a module, or stop the test saying why it does not parse.
parsed :: Text -> ParsedModule
parsed source = case parseModule defaultParserConfig "M.hs" source of
  Left problem -> error (T.unpack (describeParseError problem))
  Right m -> m

-- | Does a module parse, with what its own pragmas turn on?
parses :: Text -> Bool
parses = isRight . parseModule defaultParserConfig "M.hs"

-- | Every file of Haskell source at or under a path, in order.
haskellFilesIn :: FilePath -> IO [FilePath]
haskellFilesIn path = do
  isDir <- doesDirectoryExist path
  if not isDir
    then pure [path | any (`isSuffixOf` path) sourceExtensions]
    else do
      entries <- sort <$> quietly [] (listDirectory path)
      concat <$> traverse (haskellFilesIn . (path </>)) entries

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
  packageReader <-
    newPackageReader (plannedCompiler plan) (plannedFlags plan) []
  newResolverVia (Do #useCache) routes packageReader "." plan

-- | Every configuration of a module, with every conditional resolved.
leaves ::
  -- | The module.
  Text ->
  Either CppError [Text]
leaves = fmap (fmap snd) . answeredLeaves

-- | The configurations reached by varying one conditional at a time.
linearLeaves ::
  -- | The module.
  Text ->
  Either CppError [Text]
linearLeaves = fmap (fmap snd) . answeredLinearLeaves
