{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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
import System.FilePath (replaceExtension)
import Test.Hspec hiding (Example, after, before, example)
import Tilia.Corpus
import Tilia.Corpus.Manifest
import GHC.LanguageExtensions.Type (Extension)
import Tilia.Cpp
  ( answeredLeaves,
    answeredLinearLeaves,
    blankCpp,
    countLeaves,
    describeCppError,
    formatWithCpp,
    usesCpp,
  )
import Tilia.Diff (Colours, coloursFor, diff)
import Tilia.Pragma (effectiveExtensions, movesPositions)
import Tilia.Equivalence (commentDifference, syntaxDifference)
import Tilia.Parser
  ( ParsedModule (..),
    ParserConfig,
    describeParseError,
    parseModule,
    parserConfigFor,
  )
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Render (RenderConfig, defaultRenderConfig, renderModule)
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
        colours <- runIO coloursFor
        let run = check colours
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
              Result outcome why <- run example
              note seen (exampleName example, outcome, T.take reasonLength why)

    record seen = do
      entries <- readIORef seen
      if length entries /= length examples
        then
          putStrLn $
            "not writing "
              <> path
              <> ": "
              <> show (length entries)
              <> " of "
              <> show (length examples)
              <> " examples ran, so this run does not know what the rest do."
              <> " Regenerate without --match."
        else do
          writeManifest path (Map.fromList [(n, o) | (n, o, _) <- entries])
          writeReport reportPath entries

    note :: IORef [a] -> a -> IO ()
    note seen entry = atomicModifyIORef' seen (\es -> (entry : es, ()))

    checked = do
      manifest <- runIO (readManifest path)
      parallel $ for_ examples $ \example ->
        it (exampleName example) $ do
          Result outcome why <- run example
          case Map.lookup (exampleName example) manifest of
            Nothing -> expectationFailure (T.unpack (unrecorded outcome))
            Just expected
              | expected /= outcome ->
                  expectationFailure (T.unpack (moved expected outcome why))
              | otherwise -> case outcome of
                  Formatted -> pure ()
                  Declined -> pure ()
                  DoesNotParse -> pure ()
                  NotUtf8 -> pure ()
                  PartlyChecked -> pendingWith (T.unpack why)
                  Broken -> pendingWith (T.unpack (T.take reasonLength why))
      it "records nothing it does not have" $ do
        let had = Set.fromList (map exampleName examples)
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
data Result = Result Outcome Text

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
verdict declines (Result outcome why) = case outcome of
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

check :: Colours -> Example -> IO Result
check colours example = do
  source <- readUtf8 (exampleInput example)
  expected <- traverse readUtf8 (exampleReference example)
  case source of
    Nothing -> pure (Result NotUtf8 "")
    Just text ->
      guarded (checkPure colours (exampleName example) (exampleExtensions example) text (join expected))

-- | Read a file that is supposed to be a Haskell module.
readUtf8 :: FilePath -> IO (Maybe Text)
readUtf8 path = either (const Nothing) Just . decodeUtf8' <$> BS.readFile path

-- | Run a check, turning a crash into a failure rather than into a dead test
-- run.
guarded :: Result -> IO Result
guarded result =
  try (evaluate (forced result)) >>= \case
    Left (e :: SomeException) ->
      pure (Result Broken ("the formatter raised an error: " <> firstLine (T.pack (show e))))
    Right settled -> pure settled
  where
    forced r@(Result outcome why) = outcome `seq` T.length why `seq` r
    firstLine = T.strip . T.takeWhile (/= '\n')

-- | Everything that can be established about one example without doing any
-- more input or output.
checkPure :: Colours -> FilePath -> [Extension] -> Text -> Maybe Text -> Result
checkPure colours path package source expected
  | movesPositions source =
      Result Declined "a pragma that moves positions, which we do not rewrite"
  | usesCpp inForce source = checkCpp colours path package source expected
  | otherwise = case parseModule config path source of
      Left problem -> Result DoesNotParse (describeParseError problem)
      Right before ->
        let formatted = render before
            against name = diff colours ("input", name) source formatted
         in case parse formatted of
              Nothing ->
                Result
                  Broken
                  ( "the formatted output does not parse\n"
                      <> against "output (does not parse)"
                  )
              Just after
                | Just difference <- syntaxDifference (pmModule before) (pmModule after) ->
                    Result
                      Broken
                      ( "a different program: "
                          <> difference
                          <> "\n"
                          <> against "output"
                      )
                | Just difference <-
                    commentDifference
                      (pmModule before, pmModule after)
                      (pmComments before)
                      (pmComments after) ->
                    Result
                      Broken
                      ( "comments: "
                          <> difference
                          <> "\n"
                          <> against "output"
                      )
                | settled <- render after,
                  settled /= formatted ->
                    Result
                      Broken
                      ( "formatting is non-idempotent\n"
                          <> diff colours ("first pass", "second pass") formatted settled
                      )
                | Just reference <- expected,
                  reference /= formatted ->
                    Result
                      Broken
                      ( "does not match the corpus's expected output\n"
                          <> diff colours ("expected", "ours") reference formatted
                      )
                | otherwise -> Result Formatted ""
  where
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
checkCpp :: Colours -> FilePath -> [Extension] -> Text -> Maybe Text -> Result
checkCpp colours path package source expected = case formatWithCpp parser render path source of
  Left why -> Result Declined (describeCppError why)
  Right formatted -> case (countLeaves source, countLeaves formatted) of
    (Left why, _) -> Result Broken ("the input's configurations: " <> describeCppError why)
    (_, Left why) ->
      Result Broken ("the output's configurations: " <> describeCppError why <> "\n" <> against formatted)
    (Right went, Right came)
      | went /= came ->
          Result
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
    count :: Integer -> Text
    count = T.pack . show
    against formatted = diff colours ("input", "output") source formatted
    quantified enumerate reservation formatted =
      case (enumerate source, enumerate formatted) of
        (Left why, _) -> Result Broken ("the input's configurations: " <> describeCppError why)
        (_, Left why) ->
          Result Broken ("the output's configurations: " <> describeCppError why <> "\n" <> against formatted)
        (Right went, Right came)
          | (why : _) <- alongside went came ->
              Result Broken (why <> "\n" <> against formatted)
          | otherwise -> case formatWithCpp parser render path formatted of
              Left why ->
                Result Broken ("the output cannot be formatted again: " <> describeCppError why)
              Right settled
                | settled /= formatted ->
                    Result
                      Broken
                      ( "formatting is non-idempotent\n"
                          <> diff colours ("first pass", "second pass") formatted settled
                      )
                | Just reference <- expected,
                  reference /= formatted ->
                    Result
                      Broken
                      ( "does not match the corpus's expected output\n"
                          <> diff colours ("expected", "ours") reference formatted
                      )
                | otherwise -> maybe (Result Formatted "") (Result PartlyChecked) reservation
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
      (Nothing, _) -> Just "a configuration of the input does not parse"
      (_, Nothing) -> Just "a configuration of the output does not parse"
      (Just before, Just after)
        | Just difference <- syntaxDifference (pmModule before) (pmModule after) ->
            Just ("a different program, in one configuration: " <> difference)
        | Just difference <-
            commentDifference
              (pmModule before, pmModule after)
              (pmComments before)
              (pmComments after) ->
            Just ("comments, in one configuration: " <> difference)
        | otherwise -> Nothing

    parse = either (const Nothing) Just . parseModule parser path

    parser = parserConfigFor package
    render = renderConfigFor parser path package source

-- | How many configurations one example gets compared over.
configurationsToCheck :: Integer
configurationsToCheck = 64

-- | What to print an example's configurations with.
renderConfigFor :: ParserConfig -> FilePath -> [Extension] -> Text -> RenderConfig
renderConfigFor parser path package source =
  case parseModule parser path (blankCpp source) of
    Right whole -> exampleRenderConfig package source (pmModule whole)
    Left _ -> defaultRenderConfig
