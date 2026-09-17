{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Formatting other people's Haskell.
module Tilia.CorpusSpec (spec) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (join, unless)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import GHC.LanguageExtensions.Type (Extension)
import System.FilePath (replaceExtension)
import Test.Hspec hiding (Example, after, before, example)
import Tilia.Corpus
import Tilia.Corpus.Manifest
import Tilia.Cpp
  ( answeredLeaves,
    answeredLinearLeaves,
    blankCpp,
    countLeaves,
    describeCppError,
    formatWithCpp,
    usesCpp,
  )
import Tilia.Diff (diff)
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Equivalence (commentDifference, syntaxDifference)
import Tilia.Palette (Palette, paletteFor)
import Tilia.Parser
  ( ParseError (..),
    ParsedModule (..),
    ParserConfig,
    parseModule,
    parserConfigFor,
  )
import Tilia.Pragma (effectiveExtensions, movesPositions)
import Tilia.Render (RenderConfig, defaultRenderConfig, renderModule)
import Tilia.Source (comments)
import Tilia.Span (spanStartColumn, spanStartLine)
import Tilia.Span.Ghc (spanOfSrcSpan)
import Tilia.TestConfig (exampleRenderConfig)

spec :: Spec
spec = do
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
        it "is available" . pendingWith $
          "corpus not on this machine and could not be fetched: " <> T.unpack problem
      Right examples -> do
        palette <- runIO paletteFor
        let run = check palette
        case corpusExpectations corpus of
          Listed lists -> againstLists lists run examples
          Recorded path -> againstRecord path run examples

-- | A corpus small enough to name its exceptions in "Tilia.Corpus".
againstLists :: Lists -> (Example -> IO Result) -> [Example] -> Spec
againstLists listed run examples =
  parallel $ for_ examples $ \example ->
    it (exampleName example) $ do
      result <- run example
      case verdict (Set.member (exampleName example) declines) result of
        Passes -> pure ()
        Fails why -> expectationFailure (T.unpack why)
        Reserved why -> pendingWith (T.unpack why)
  where
    declines = Set.fromList (expectDeclined listed)

-- | A corpus checked against a generated record of what it does.
againstRecord :: FilePath -> (Example -> IO Result) -> [Example] -> Spec
againstRecord path run examples = do
  accept <- runIO accepting
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
              note seen (exampleName example, Entry outcome digest, T.take reasonLength why)

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
      parallel $ for_ examples $ \example ->
        it (exampleName example) $ do
          Result outcome why digest <- run example
          case Map.lookup (exampleName example) manifest of
            Nothing -> expectationFailure (T.unpack (unrecorded outcome))
            Just expected
              | entryOutcome expected /= outcome ->
                  expectationFailure (T.unpack (moved (entryOutcome expected) outcome why))
              | entryDigest expected /= digest ->
                  expectationFailure (T.unpack (rewritten (entryDigest expected) digest))
              | otherwise -> case outcome of
                  Formatted -> pure ()
                  Declined -> pure ()
                  DoesNotParse -> pure ()
                  NotUtf8 -> pure ()
                  PartlyChecked -> pendingWith (T.unpack why)
                  Broken -> pendingWith (T.unpack (T.take reasonLength why))
      it "records nothing it does not have" $ do
        let had = Set.fromList (fmap exampleName examples)
            gone = [n | n <- Map.keys manifest, not (Set.member n had)]
        unless (null gone) . expectationFailure $
          show (length gone)
            <> " entries name examples this corpus does not have, starting with "
            <> unwords (take 5 gone)
            <> regenerate

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
  -- | Does the corpus say this one should be declined?
  Bool ->
  Result ->
  Verdict
verdict declines (Result outcome why _) = case outcome of
  Broken -> Fails why
  NotUtf8 -> Fails (unlisted "is not UTF-8")
  DoesNotParse -> Fails (unlisted ("does not parse, at " <> why))
  Declined
    | declines -> Passes
    | otherwise -> Fails "the formatter declined this, and the corpus does not say it should"
  PartlyChecked
    | declines -> Fails wasNotDeclined
    | otherwise -> Reserved why
  Formatted
    | declines -> Fails wasNotDeclined
    | otherwise -> Passes
  where
    wasNotDeclined = "the corpus says this should be declined, and it was not"
    unlisted what =
      "this " <> what <> ", and the corpus does not list it under expectSkip"

check :: Palette -> Example -> IO Result
check palette example = do
  source <- readUtf8 (exampleInput example)
  expected <- traverse readUtf8 (exampleReference example)
  case source of
    Nothing -> pure (Result NotUtf8 "" noDigest)
    Just text ->
      guarded (checkPure palette (exampleName example) (exampleExtensions example) text (join expected))

-- | Read a file that is supposed to be a Haskell module.
readUtf8 :: FilePath -> IO (Maybe Text)
readUtf8 path = either (const Nothing) Just . decodeUtf8' <$> BS.readFile path

-- | Run a check, turning a crash into a failure rather than into a dead test
-- run.
guarded :: Result -> IO Result
guarded result =
  try (evaluate (forced result)) >>= \case
    Left (e :: SomeException) ->
      pure (Result Broken ("the formatter raised an error: " <> firstLine (T.pack (show e))) noDigest)
    Right settled -> pure settled
  where
    forced r =
      resultOutcome r
        `seq` T.length (resultWhy r)
        `seq` T.length (resultDigest r)
        `seq` r
    firstLine = T.strip . T.takeWhile (/= '\n')

-- | Everything that can be established about one example without doing any
-- more input or output.
checkPure :: Palette -> FilePath -> [Extension] -> Text -> Maybe Text -> Result
checkPure palette path package source expected
  | movesPositions source =
      Result Declined "a pragma that moves positions, which we do not rewrite" noDigest
  | usesCpp inForce source = checkCpp palette path package source expected
  | otherwise = case parseModule config path source of
      Left problem -> Result DoesNotParse (parseProblem problem) noDigest
      Right before ->
        let formatted = render before
            against name = diff palette ("input", name) source formatted
         in case parse formatted of
              Nothing ->
                told
                  formatted
                  Broken
                  ( "the formatted output does not parse\n"
                      <> against "output (does not parse)"
                  )
              Just after
                | Just difference <- syntaxDifference (pmModule before) (pmModule after) ->
                    told
                      formatted
                      Broken
                      ( "a different program: "
                          <> difference
                          <> "\n"
                          <> against "output"
                      )
                | Just difference <-
                    commentDifference
                      (pmModule before, pmModule after)
                      (comments (pmSource before))
                      (comments (pmSource after)) ->
                    told
                      formatted
                      Broken
                      ( "comments: "
                          <> difference
                          <> "\n"
                          <> against "output"
                      )
                | settled <- render after,
                  settled /= formatted ->
                    told
                      formatted
                      Broken
                      ( "formatting is non-idempotent\n"
                          <> diff palette ("first pass", "second pass") formatted settled
                      )
                | Just reference <- expected,
                  reference /= formatted ->
                    told
                      formatted
                      Broken
                      ( "does not match the corpus's expected output\n"
                          <> diff palette ("expected", "ours") reference formatted
                      )
                | otherwise -> told formatted Formatted ""
  where
    told formatted outcome why = Result outcome why (digestOf formatted)
    config = parserConfigFor package
    inForce = effectiveExtensions package source
    parse = either (const Nothing) Just . parseModule config path
    render parsed =
      printDoc
        defaultRenderOptions
        (renderModule (exampleRenderConfig package source (pmModule parsed)) parsed)

----------------------------------------------------------------------------
-- Checking an example that involved the preprocessor

-- | Everything that can be established about an example with conditionals in
-- it.
checkCpp :: Palette -> FilePath -> [Extension] -> Text -> Maybe Text -> Result
checkCpp palette path package source expected = case formatWithCpp parser render path source of
  Left why -> Result Declined (describeCppError why) noDigest
  Right formatted -> case (countLeaves source, countLeaves formatted) of
    (Left why, _) ->
      told formatted Broken ("the input's configurations: " <> describeCppError why)
    (_, Left why) ->
      told formatted Broken ("the output's configurations: " <> describeCppError why <> "\n" <> against formatted)
    (Right went, Right came)
      | went /= came ->
          told
            formatted
            Broken
            ( "formatting changed how many configurations there are, from "
                <> count went
                <> " to "
                <> count came
                <> "\n"
                <> against formatted
            )
      | went <= configurationsToCheck -> quantified answeredLeaves Nothing formatted
      | otherwise ->
          quantified
            answeredLinearLeaves
            ( Just
                ( count went
                    <> " configurations is more than the "
                    <> count configurationsToCheck
                    <> " this checks, so only the ones varying a single"
                    <> " conditional were compared"
                )
            )
            formatted
  where
    told formatted outcome why = Result outcome why (digestOf formatted)
    count :: Integer -> Text
    count = T.pack . show
    against formatted = diff palette ("input", "output") source formatted
    quantified enumerate reservation formatted =
      case (enumerate source, enumerate formatted) of
        (Left why, _) ->
          told formatted Broken ("the input's configurations: " <> describeCppError why)
        (_, Left why) ->
          told formatted Broken ("the output's configurations: " <> describeCppError why <> "\n" <> against formatted)
        (Right went, Right came)
          | (why : _) <- alongside went came ->
              told formatted Broken (why <> "\n" <> against formatted)
          | otherwise -> case formatWithCpp parser render path formatted of
              Left why ->
                told formatted Broken ("the output cannot be formatted again: " <> describeCppError why)
              Right settled
                | settled /= formatted ->
                    told
                      formatted
                      Broken
                      ( "formatting is non-idempotent\n"
                          <> diff palette ("first pass", "second pass") formatted settled
                      )
                | Just reference <- expected,
                  reference /= formatted ->
                    told
                      formatted
                      Broken
                      ( "does not match the corpus's expected output\n"
                          <> diff palette ("expected", "ours") reference formatted
                      )
                | otherwise ->
                    maybe (told formatted Formatted "") (told formatted PartlyChecked) reservation
    alongside went came =
      [ why
      | (answers, before) <- went,
        why <- case Map.lookup answers output of
          Nothing -> ["a configuration of the input the output does not have"]
          Just after -> maybe [] pure (sameProgram before after)
      ]
        <> [ "a configuration of the output the input does not have"
           | any (\(answers, _) -> not (Map.member answers input)) came
           ]
      where
        output = Map.fromList came
        input = Map.fromList went

    sameProgram went came = case (parse went, parse came) of
      (Left problem, _) ->
        Just ("a configuration of the input does not parse: " <> parseProblem problem)
      (_, Left problem) ->
        Just
          ( "a configuration of the output does not parse: "
              <> parseProblem problem
              <> "\n"
              <> linesAround came problem
          )
      (Right before, Right after)
        | Just difference <- syntaxDifference (pmModule before) (pmModule after) ->
            Just ("a different program, in one configuration: " <> difference)
        | Just difference <-
            commentDifference
              (pmModule before, pmModule after)
              (comments (pmSource before))
              (comments (pmSource after)) ->
            Just ("comments, in one configuration: " <> difference)
        | otherwise -> Nothing

    parse = parseModule parser path

    parser = parserConfigFor package
    render = renderConfigFor parser path package source

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

-- | The lines of a configuration around the one a parse error names.
linesAround :: Text -> ParseError -> Text
linesAround text problem = case spanStartLine <$> spanOfSrcSpan (peSpan problem) of
  Nothing -> T.empty
  Just line ->
    T.unlines
      [ (if n == line then "> " else "  ") <> T.pack (show n) <> "  " <> l
      | (n, l) <- zip [1 :: Int ..] (T.lines text),
        abs (n - line) <= 4
      ]

-- | How many configurations one example gets compared over.
configurationsToCheck :: Integer
configurationsToCheck = 64

-- | What to print an example's configurations with.
renderConfigFor :: ParserConfig -> FilePath -> [Extension] -> Text -> RenderConfig
renderConfigFor parser path package source =
  case parseModule parser path (blankCpp source) of
    Right whole -> exampleRenderConfig package source (pmModule whole)
    Left _ -> defaultRenderConfig
