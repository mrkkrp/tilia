{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Formatting other people's Haskell.
module Tilia.CorpusSpec (spec) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (join, unless)
import Data.ByteString qualified as BS
import Data.Choice (isTrue, pattern Do, pattern Don't)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import System.Directory (createDirectoryIfMissing)
import System.FilePath (replaceExtension, (</>))
import System.IO (stdout)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec hiding (Example, after, before, example)

import Tilia.Corpus
import Tilia.Corpus.Manifest
import Tilia.Cpp (countLeaves, describeCppError, usesCpp)
import Tilia.Diff qualified as Diff
import Tilia.Format (FormatError (..), describeFormatError, refused)
import Tilia.Palette (Color (Meta), Palette (Plain), paint, paletteFor)
import Tilia.Parser (ParseError (..))
import Tilia.Pragma (Defaults)
import Tilia.Span (spanStartColumn, spanStartLine)
import Tilia.Span.Ghc (spanOfSrcSpan)
import Tilia.TestConfig
  ( configurationsToCheck,
    effectiveExtensionsFor,
    formatExample,
  )
import Tilia.TestUtils
  ( switchedOn,
    withEnvironment,
  )

spec :: Spec
spec = do
  describe "obtaining Hackage releases" $
    it "takes in only the releases listed, whatever else is in the cache" $
      withSystemTempDirectory "tilia-corpus" $ \cache -> do
        for_ ["listed-1.0", "unlisted-1.0"] $ \release -> do
          createDirectoryIfMissing True (cache </> "hackage" </> release)
          writeFile
            (cache </> "hackage" </> release </> "M.hs")
            "module M where\n"
        obtained <-
          withEnvironment [("TILIA_CORPUS_DIR", cache)] . obtain $
            hackagePackages{corpusSource = HackageReleases ["listed-1.0"]}
        fmap (fmap exampleName) obtained `shouldBe` Right ["listed-1.0/M.hs"]
  corpusSpec vendoredExamples
  corpusSpec ormoluExamples
  corpusSpec ghcTestSuite
  corpusSpec hackagePackages

-- | Every example of one corpus.
corpusSpec :: Corpus -> Spec
corpusSpec corpus =
  describe (corpusName corpus) $
    runIO (obtain corpus) >>= \case
      Left problem ->
        it "is available" . expectationFailure $
          "corpus not on this machine and could not be fetched: " <> T.unpack problem
      Right examples -> do
        palette <- runIO (paletteFor stdout)
        case corpusExpectations corpus of
          Listed lists -> againstLists lists (check palette) examples
          -- Plain, so that what a record says does not depend on whether the
          -- run that wrote it printed to a terminal.
          Recorded path -> againstRecord path (check Plain) examples

-- | A corpus small enough to name its exceptions in "Tilia.Corpus".
againstLists :: Lists -> (Example -> IO Result) -> [Example] -> Spec
againstLists listed run examples =
  parallel $ for_ examples $ \example ->
    it (exampleName example) $ do
      result <- run example
      let holds = Set.member (exampleName example)
      case verdict (holds unreadable) (holds declines) result of
        Passes -> pure ()
        Fails why -> expectationFailure (T.unpack why)
        Reserved why -> pendingWith (T.unpack why)
  where
    unreadable = Set.fromList (expectUnreadable listed)
    declines = Set.fromList (expectDeclined listed)

-- | A corpus checked against a generated record of what it does.
againstRecord :: FilePath -> (Example -> IO Result) -> [Example] -> Spec
againstRecord path run examples = do
  accept <- runIO (switchedOn "TILIA_CORPUS_ACCEPT")
  if accept then accepted else checked
  where
    reportPath = replaceExtension path ".report"

    accepted = do
      seen <- runIO (newIORef [])
      afterAll_ (record seen) $
        parallel $
          for_ examples $ \example ->
            it (exampleName example) $ do
              Result outcome why digest <- run example
              note
                seen
                ( exampleName example,
                  Entry outcome digest,
                  T.take reasonLength why
                )

    record seen = do
      noted <- readIORef seen
      if length noted /= length examples
        then
          putStrLn $
            "not writing "
              <> path
              <> ": "
              <> show (length noted)
              <> " of "
              <> show (length examples)
              <> " examples ran, so this run does not know what the rest do."
              <> " Regenerate without --match."
        else do
          writeManifest path (Map.fromList [(n, e) | (n, e, _) <- noted])
          writeReport reportPath [(n, entryOutcome e, w) | (n, e, w) <- noted]

    note :: IORef [a] -> a -> IO ()
    note seen entry = atomicModifyIORef' seen (\es -> (entry : es, ()))

    checked = do
      manifest <- runIO (readManifest path)
      report <- runIO (readReport reportPath)
      parallel $ for_ examples $ \example ->
        it (exampleName example) $ do
          Result outcome why digest <- run example
          case Map.lookup (exampleName example) manifest of
            Nothing -> expectationFailure (T.unpack (unrecorded outcome))
            Just expected
              | entryOutcome expected /= outcome ->
                  expectationFailure
                    (T.unpack (moved (entryOutcome expected) outcome why))
              | entryDigest expected /= digest ->
                  expectationFailure
                    (T.unpack (rewritten (entryDigest expected) digest))
              | Just problem <-
                  misreported
                    (Map.lookup (exampleName example) report)
                    outcome
                    (reasonLines (T.take reasonLength why)) ->
                  expectationFailure (T.unpack problem)
              | otherwise -> case outcome of
                  Formatted -> pure ()
                  Declined -> pure ()
                  DoesNotParse -> pure ()
                  NotUtf8 -> pure ()
                  PartlyChecked -> pendingWith (T.unpack why)
                  Broken -> pendingWith (T.unpack (T.take reasonLength why))
      it "records nothing it does not have" $ do
        let gone = [n | n <- Map.keys manifest, not (Set.member n had)]
        unless (null gone) . expectationFailure $
          show (length gone)
            <> " entries name examples this corpus does not have, starting with "
            <> unwords (take 5 gone)
            <> regenerate
      it "reports nothing it does not have" $ do
        let gone = [n | n <- Map.keys report, not (Set.member n had)]
        unless (null gone) . expectationFailure $
          reportPath
            <> " gives reasons for "
            <> show (length gone)
            <> " examples this corpus does not have, starting with "
            <> unwords (take 5 gone)
            <> regenerate

    had = Set.fromList (fmap exampleName examples)

    misreported filed outcome reason = case (outcome, filed) of
      (Formatted, Nothing) -> Nothing
      (Formatted, Just _) ->
        Just
          (T.pack reportPath <> " gives a reason why this is not formatted, and it is" <> T.pack regenerate)
      (_, Just (filedUnder, given))
        | filedUnder == outcome && given == reason -> Nothing
      _ ->
        Just $
          T.pack reportPath
            <> " says otherwise why this "
            <> outcomeName outcome
            <> ", which is now:\n"
            <> T.unlines reason
            <> T.pack regenerate

    unrecorded outcome =
      "this is not in "
        <> T.pack path
        <> ", and it "
        <> outcomeName outcome
        <> T.pack regenerate

    rewritten was now =
      T.pack path
        <> " says this comes out as "
        <> was
        <> ", and it comes out as "
        <> now
        <> T.pack regenerate

    moved expected outcome why =
      T.pack path
        <> " says this "
        <> outcomeName expected
        <> ", and it "
        <> outcomeName outcome
        <> (if T.null why then "" else ": " <> why)
        <> T.pack regenerate

    regenerate =
      "\n\nIf that is the intended change, regenerate the record:"
        <> "\n    TILIA_CORPUS_ACCEPT=1 cabal test"

-- | How much of an example's reason a record keeps.
reasonLength :: Int
reasonLength = 2000

----------------------------------------------------------------------------
-- Checking one example

-- | What running the formatter over one example established, and why.
data Result = Result
  { -- | The outcome.
    resultOutcome :: Outcome,
    -- | Explanation in text.
    resultWhy :: Text,
    -- | A digest of what the formatter wrote, or
    -- 'Tilia.Corpus.Manifest.noDigest' where it wrote nothing.
    resultDigest :: Text
  }

-- | What the runner should do about what one example produced.
data Verdict
  = -- | Nothing to report.
    Passes
  | -- | Something is wrong, and this is what.
    Fails Text
  | -- | Neither: everything that was asked of it held, and something worth
    -- asking went unasked. Reported rather than passed over, so that the
    -- number of examples whose properties were only partly established is
    -- visible in the summary instead of implied by its absence.
    Reserved Text

-- | What is to be said about what one example produced.
verdict ::
  -- | Does the corpus say this one cannot be read?
  Bool ->
  -- | Does the corpus say this one should be declined?
  Bool ->
  Result ->
  Verdict
verdict True _ (Result outcome _ _)
  | outcome `elem` [DoesNotParse, NotUtf8] = Passes
  | otherwise = Fails "the corpus says this cannot be read, and it was read"
verdict False declines (Result outcome why _) = case outcome of
  Broken -> Fails why
  NotUtf8 -> Fails (unlisted "is not UTF-8")
  DoesNotParse -> Fails (unlisted ("does not parse, at " <> why))
  Declined
    | declines -> Passes
    | otherwise ->
        Fails
          "the formatter declined this, and the corpus does not say it should"
  PartlyChecked
    | declines -> Fails wasNotDeclined
    | otherwise -> Reserved why
  Formatted
    | declines -> Fails wasNotDeclined
    | otherwise -> Passes
  where
    wasNotDeclined = "the corpus says this should be declined, and it was not"
    unlisted what =
      "this "
        <> what
        <> ", and the corpus does not list it under expectUnreadable"

-- | What formatting one example, with every check, establishes.
check ::
  -- | How to color the diffs it reports.
  Palette ->
  -- | The example.
  Example ->
  IO Result
check palette example = do
  source <- readUtf8 (exampleInput example)
  expected <- traverse readUtf8 (exampleReference example)
  case source of
    Nothing -> pure (Result NotUtf8 "" noDigest)
    Just text -> guarded $ do
      let package = exampleExtensions example
          format checking =
            formatExample
              checking
              package
              (exampleOwnModules example)
              (exampleName example)
          unchecked = do
            printed <- format (Don't #check) text
            (printed,) <$> either (pure . Left) (format (Don't #check)) printed
      checked <- format (Do #check) text
      passes <- case checked of
        Left NotEquivalent{} -> Just <$> unchecked
        Left NotIdempotent{} -> Just <$> unchecked
        _ -> pure Nothing
      pure (checkPure palette package text (join expected) checked passes)

-- | Read a file that is supposed to be a Haskell module.
readUtf8 :: FilePath -> IO (Maybe Text)
readUtf8 path = either (const Nothing) Just . decodeUtf8' <$> BS.readFile path

-- | A unified diff, cut short after 60 lines, past which it stops being
-- read.
diff :: Palette -> (Text, Text) -> Text -> Text -> Text
diff palette names before after
  | length body > room =
      T.intercalate "\n" (heading <> take room body <> [omitted])
  | otherwise = whole
  where
    whole = Diff.diff palette names before after
    (heading, body) = splitAt 2 (T.lines whole)
    room = 60
    omitted =
      paint palette Meta $
        "… and " <> T.pack (show (length body - room)) <> " more lines"

-- | Run a check, turning a crash into a failure rather than into a dead test
-- run.
guarded ::
  -- | The check.
  IO Result ->
  IO Result
guarded run =
  try (run >>= evaluate . forced) >>= \case
    Left (e :: SomeException) ->
      pure
        ( Result
            Broken
            ("the formatter raised an error: " <> firstLine (T.pack (show e)))
            noDigest
        )
    Right settled -> pure settled
  where
    forced r =
      resultOutcome r
        `seq` T.length (resultWhy r)
        `seq` T.length (resultDigest r)
        `seq` r
    firstLine = T.strip . T.takeWhile (/= '\n')

-- | What a refusal or a failure to format an example comes to.
gaveUp ::
  -- | Why formatting gave up.
  FormatError ->
  Result
gaveUp = \case
  NotParsed problem -> Result DoesNotParse (parseProblem problem) noDigest
  CppUnsupported _ why -> Result Declined (describeCppError why) noDigest
  e
    | refused e -> Result Declined (describeFormatError Plain e) noDigest
    | otherwise -> Result Broken (describeFormatError Plain e) noDigest

-- | Everything that can be established about one example from formatting
-- it with every check.
checkPure ::
  -- | How to color the diffs it reports.
  Palette ->
  -- | What the package around it puts in force, if it is a module of one.
  Maybe Defaults ->
  -- | The example.
  Text ->
  -- | The expected output, if the corpus has one.
  Maybe Text ->
  -- | Formatting the example, checked.
  Either FormatError Text ->
  -- | Where a check failed, formatting the example and then what that
  -- printed, unchecked.
  Maybe (Either FormatError Text, Either FormatError Text) ->
  Result
checkPure palette package source expected checked unchecked =
  case (checked, unchecked) of
    (Right formatted, _)
      | Just reference <- expected,
        reference /= formatted ->
          told
            formatted
            Broken
            ( "does not match the corpus's expected output\n"
                <> diff palette ("expected", "ours") reference formatted
            )
      | Just went <- beyond ->
          told
            formatted
            PartlyChecked
            ( count went
                <> " configurations is more than the "
                <> count (toInteger configurationsToCheck)
                <> " this checks, so only the ones varying a single"
                <> " conditional were compared"
            )
      | otherwise -> told formatted Formatted ""
    (Left e@NotEquivalent{}, Just (Right printed, _)) ->
      told
        printed
        Broken
        ( describeFormatError Plain e
            <> "\n"
            <> diff palette ("input", "output") source printed
        )
    (Left e@NotIdempotent{}, Just (Right printed, Right settled)) ->
      told
        printed
        Broken
        ( describeFormatError Plain e
            <> "\n"
            <> diff palette ("first pass", "second pass") printed settled
        )
    (Left e, _) -> gaveUp e
  where
    told formatted outcome why = Result outcome why (digestOf formatted)
    count :: Integer -> Text
    count = T.pack . show
    beyond = case countLeaves source of
      Right went
        | isTrue (usesCpp (effectiveExtensionsFor package source) source),
          went > toInteger configurationsToCheck ->
            Just went
      _ -> Nothing

-- | Why a parse failed, and where in the file, but not which file.
--
-- The example being reported already names it, and 'describeParseError'
-- opens with the path in full.
parseProblem :: ParseError -> Text
parseProblem problem = at <> peProblem problem
  where
    at = case spanOfSrcSpan (peSpan problem) of
      Nothing -> T.empty
      Just s ->
        T.pack (show (spanStartLine s))
          <> ":"
          <> T.pack (show (spanStartColumn s))
          <> ": "
