{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Helpers several specs share.
module Tilia.TestUtils
  ( withEnvironment,
    parserConfigFor,
    defaultParserConfig,
    effectiveExtensionsFor,
    parsed,
    commentsIn,
    unreadOnly,
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
import Data.Char (ord)
import Data.Choice (pattern Do)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Driver.Session qualified as GHC
import GHC.LanguageExtensions.Type (Extension (..))
import System.Environment (lookupEnv, setEnv, unsetEnv)
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
import Tilia.Fixity (ModuleChain (..))
import Tilia.Fixity.Plan (Resolver, Route, newResolverVia)
import Tilia.Parser
  ( ParsedModule (..),
    ParserConfig (..),
    describeParseError,
    parseModule,
  )
import Tilia.Pragma (effectiveExtensions)
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

-- | What to parse a module with, given what its package puts in force, or
-- 'Nothing' for a module of no package, which is read as GHC2021.
parserConfigFor :: Maybe [Extension] -> ParserConfig
parserConfigFor =
  ParserConfig . fromMaybe (GHC.languageExtensions (Just GHC.GHC2021))

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
effectiveExtensionsFor :: Maybe [Extension] -> Text -> [Extension]
effectiveExtensionsFor = effectiveExtensions . fromMaybe [ImplicitPrelude]

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
  packageReader <- newPackageReader (plannedCompiler plan)
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
      Nothing | not (null (unconditionalErrors source)) -> Right []
      Nothing -> (\t -> [(answers, t)]) <$> resolved source
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
      Nothing -> (\t -> [(answers, t)]) <$> resolved source
      Just c -> case zip [0 ..] (cfgTexts c) of
        [] -> Right []
        (i, first) : rest ->
          (<>)
            <$> go (Map.insert (cfgGuards c) i answers) first
            <*> traverse (held answers (cfgGuards c)) rest
      where
        held before gs (i, t) = answeredBaseline (Map.insert gs i before) t

-- | The configuration in which every question still to be asked takes its
-- first branch, and the answers that gives.
answeredBaseline :: Assignment -> Text -> Either CppError (Assignment, Text)
answeredBaseline answers source = case configurations source of
  Nothing -> (answers,) <$> resolved source
  Just c -> case cfgTexts c of
    [] -> Right (answers, source)
    t : _ -> answeredBaseline (Map.insert (cfgGuards c) 0 answers) t
