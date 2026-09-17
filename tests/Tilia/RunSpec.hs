{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Running the formatter over a set of files.
module Tilia.RunSpec (spec) where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.IORef
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import GHC.Clock (getMonotonicTime)
import System.Directory (getModificationTime)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Tilia.Cabal.Package (PackageProblem (..))
import Tilia.Cpp (CppError (..))
import Tilia.Fixity (ModuleChain (..), OpName (..), Unknown (..))
import Tilia.Format (FormatError (..), formatErrorExitCode, refused)
import Tilia.Palette (Color (Bad), Palette (..))
import Tilia.Run
import Tilia.Utils (lineWidth, visibleLength, wrapTo)

spec :: Spec
spec = do
  describe "telling a refusal from a failure" $ do
    it "counts what we would not touch as a refusal" $
      fmap
        refused
        [ PositionPragmas "A.hs",
          CppUnsupported "A.hs" UnsplittableConditional,
          UnknownFixity "A.hs" []
        ]
        `shouldBe` [True, True, True]

    it "counts what we could not read or make sense of as a failure" $
      fmap
        refused
        [ Unreadable "A.hs" "no such file",
          NoPackage "A.hs" NoPackageFile,
          NoProject "A.hs",
          NoBuildPlan "." "cabal said no",
          NotEquivalent "A.hs" "f = 1 became f = 2",
          NotIdempotent "A.hs" "line 12 differs"
        ]
        `shouldBe` [False, False, False, False, False, False]

  describe "what became of a file" $ do
    it "counts a rewrite as a difference and nothing else" $
      fmap differs [Changed "a" "b", Unchanged, decline, failure]
        `shouldBe` [True, False, False, False]

    it "keeps refusals and failures apart" $ do
      fmap declined [decline, failure, Unchanged] `shouldBe` [True, False, False]
      fmap failed [decline, failure, Unchanged] `shouldBe` [False, True, False]

  describe "the status a run leaves behind" $ do
    it "has none to give when nothing failed" $
      exitCodeOf [("A.hs", Unchanged), ("B.hs", decline), ("C.hs", Changed "a" "b")]
        `shouldBe` Nothing

    it "gives the code of the failure when there is one" $
      exitCodeOf [("A.hs", failure)]
        `shouldBe` Just (formatErrorExitCode (Unreadable "A.hs" "gone"))

    it "gives the lowest code when there are several" $
      exitCodeOf [("A.hs", failure), ("B.hs", otherFailure)]
        `shouldBe` Just (min (formatErrorExitCode unreadable) (formatErrorExitCode unclaimed))

    it "does not depend on the order the failures came back in" $
      exitCodeOf [("A.hs", failure), ("B.hs", otherFailure)]
        `shouldBe` exitCodeOf [("B.hs", otherFailure), ("A.hs", failure)]

    it "is not swayed by a refusal, however many there are" $
      exitCodeOf [("A.hs", decline), ("B.hs", decline)] `shouldBe` Nothing

  describe "the summary an inplace run prints" $ do
    it "counts the files it formatted, indented, by extension" $
      reportOut (inplaceReport Plain [("A.hs", Unchanged), ("B.hs", Changed "a" "b")])
        `shouldBe` ["  [✓] Formatted 2 .hs files"]

    it "counts each extension on its own line, in a settled order" $
      reportOut
        ( inplaceReport
            Plain
            [("C.hsig", Unchanged), ("A.hs", Unchanged), ("B.hs-boot", Unchanged)]
        )
        `shouldBe` [ "  [✓] Formatted 1 .hs file",
                     "  [✓] Formatted 1 .hs-boot file",
                     "  [✓] Formatted 1 .hsig file"
                   ]

    it "says file rather than files when there is one" $
      reportOut (inplaceReport Plain [("A.hs", Unchanged)])
        `shouldBe` ["  [✓] Formatted 1 .hs file"]

    it "counts neither a refusal nor a failure among the formatted" $
      reportOut (inplaceReport Plain [("A.hs", Unchanged), ("B.hs", decline), ("C.hs", failure)])
        `shouldBe` ["  [✓] Formatted 1 .hs file"]

    it "says nothing at all about a run with no files" $
      inplaceReport Plain [] `shouldBe` Report [] []

  describe "the files a run did not format" $ do
    let mixed =
          [ ("b.hs", decline),
            ("a.hs", Failed (Unreadable "a.hs" "no such file")),
            ("c.hs-boot", decline)
          ]

    it "tallies refusals and failures apart, and puts refusals first" $
      filter (T.isInfixOf "] ") (reportErr (inplaceReport Plain mixed))
        `shouldBe` [ "  [=] Declined 1 .hs file",
                     "  [=] Declined 1 .hs-boot file",
                     "  [✗] Failed 1 .hs file"
                   ]

    it "gives a reason for every one of them" $
      length (filter opensAReason (reportErr (inplaceReport Plain mixed)))
        `shouldBe` 3

    it "says what a file that would not settle did" $
      reportErr
        (inplaceReport Plain [("A.hs", Failed (NotIdempotent "A.hs" "line 12 differs"))])
        `shouldSatisfy` any (T.isInfixOf "A.hs is not idempotent: line 12 differs")

    it "names the file in the reason, once" $
      reportErr (inplaceReport Plain mixed)
        `shouldSatisfy` any (T.isInfixOf "cannot read a.hs: no such file")

    it "orders the reasons by file, not by arrival" $
      reportErr (inplaceReport Plain mixed)
        `shouldBe` reportErr (inplaceReport Plain (reverse mixed))

    it "opens every case with a bullet" $
      filter opensAReason (reportErr (inplaceReport Plain mixed))
        `shouldSatisfy` all (T.isPrefixOf "    · ")

    it "lines a wrapped case up with the text of its bullet" $
      reportErr (inplaceReport Plain [("A.hs", Failed (Unreadable "A.hs" (T.replicate 20 "and more words ")))])
        `shouldSatisfy` \case
          (_tallyLine : opening : continuation : _) ->
            T.isPrefixOf "    · " opening
              && T.takeWhile (== ' ') continuation == "      "
          _ -> False

    it "lays an unsettled operator at the door of the run, not of the module" $
      flattened (inplaceReport Plain [("A.hs", missing "Criterion.Main")])
        `shouldSatisfy` T.isInfixOf
          "the fixity of <|> may be declared in Criterion.Main, which this run could not read"

    it "counts the modules when the answer could be in more than one" $
      flattened (inplaceReport Plain [("A.hs", missingIn ["Criterion.Main", "Test.Tasty"])])
        `shouldSatisfy` T.isInfixOf
          "may be declared in Criterion.Main or Test.Tasty, neither of which this run could read"

    it "names the module reading stopped at, not only the import above it" $
      flattened
        (inplaceReport Plain [("A.hs", missingThrough [ModuleChain ("Test.Hspec" :| ["Test.QuickCheck.Property"])])])
        `shouldSatisfy` T.isInfixOf
          "may be declared in Test.Hspec → Test.QuickCheck.Property, which this run could not read"

    it "gives one answer once however many operators share it" $
      flattened (inplaceReport Plain [("A.hs", missingTwice "Criterion.Main")])
        `shouldSatisfy` T.isInfixOf
          "the fixities of <|> and <+> may be declared in Criterion.Main, which this run could not read"

    it "puts a comma before the and once there are three of them" $
      flattened (inplaceReport Plain [("A.hs", missingAll ["<|>", "<+>", "<*>"] "Criterion.Main")])
        `shouldSatisfy` T.isInfixOf "the fixities of <|>, <+>, and <*> may be declared in"

    it "leaves a pair without one" $
      flattened (inplaceReport Plain [("A.hs", missingAll ["<|>", "<+>"] "Criterion.Main")])
        `shouldSatisfy` T.isInfixOf "the fixities of <|> and <+> may be declared in"

    it "counts each answer's operators rather than the file's" $
      flattened (inplaceReport Plain [("A.hs", twoAnswers)])
        `shouldSatisfy` T.isInfixOf
          "the fixity of <|> may be declared in Criterion.Main, which this run \
          \could not read, and the fixity of <+> is declared differently by two \
          \modules in scope"

    it "keeps all of it off the stream the summary goes to" $
      reportOut (inplaceReport Plain mixed) `shouldBe` []

    it "says the same about them in either command" $
      reportErr (checkReport Plain mixed) `shouldBe` reportErr (inplaceReport Plain mixed)

  describe "what a check run prints" $ do
    it "shows a diff for a file that would change" $
      reportOut (checkReport Plain [("A.hs", Changed "one\n" "two\n")])
        `shouldSatisfy` any (T.isInfixOf "--- a/A.hs")

    it "heads it the way git heads one" $
      reportOut (checkReport Plain [("A.hs", Changed "one\n" "two\n")])
        `shouldSatisfy` any (T.isInfixOf "diff --git a/A.hs b/A.hs")

    it "shows the removal and the addition" $ do
      let shown = T.unlines (reportOut (checkReport Plain [("A.hs", Changed "one\n" "two\n")]))
      shown `shouldSatisfy` T.isInfixOf "-one"
      shown `shouldSatisfy` T.isInfixOf "+two"

    it "says nothing on standard output about a file it did not format" $
      reportOut (checkReport Plain [("A.hs", Unchanged), ("B.hs", decline), ("C.hs", failure)])
        `shouldBe` []

  describe "color" $ do
    it "leaves everything bare when there is nobody to see it" $
      reportOut (inplaceReport Plain [("A.hs", Unchanged)])
        `shouldSatisfy` all (not . T.isInfixOf "\ESC")

    it "colors the tick and not the brackets around it" $
      reportOut (inplaceReport Colors [("A.hs", Unchanged)])
        `shouldSatisfy` any (T.isInfixOf "[\ESC[32m✓\ESC[0m]")

    it "colors the equals sign yellow" $
      reportErr (inplaceReport Colors [("A.hs", decline)])
        `shouldSatisfy` any (T.isInfixOf "[\ESC[33m=\ESC[0m]")

    it "sets the extension in the summary in bold" $
      reportOut (inplaceReport Colors [("A.hs", Unchanged)])
        `shouldSatisfy` any (T.isInfixOf "\ESC[1m.hs\ESC[0m")

    it "sets the file a case is about in bold, and nothing around it" $
      reportErr (inplaceReport Colors [("src/A.hs", failureAbout "src/A.hs")])
        `shouldSatisfy` any (T.isInfixOf "\ESC[1msrc/A.hs\ESC[0m")

    it "leaves the file bare when there is nobody to see it" $
      reportErr (inplaceReport Plain [("src/A.hs", failureAbout "src/A.hs")])
        `shouldSatisfy` all (not . T.isInfixOf "\ESC")

    it "sets an operator a message names in cyan" $
      reportErr (inplaceReport Colors [("A.hs", about "<|>")])
        `shouldSatisfy` any (T.isInfixOf "\ESC[36m<|>\ESC[0m")

    it "matches an operator as a whole word and not as a fragment" $ do
      let shown = T.unlines (reportErr (inplaceReport Colors [("A.hs", about ".")]))
      T.count "\ESC[36m" shown `shouldBe` 1
      shown `shouldSatisfy` T.isInfixOf "\ESC[1mA.hs\ESC[0m"

    it "leaves the operator bare when there is nobody to see it" $
      reportErr (inplaceReport Plain [("A.hs", about "<|>")])
        `shouldSatisfy` all (not . T.isInfixOf "\ESC")

    it "sets a module a message names in bold" $
      reportErr (inplaceReport Colors [("A.hs", missing "Criterion.Main")])
        `shouldSatisfy` any (T.isInfixOf "\ESC[1mCriterion.Main\ESC[0m")

    it "colors a module even where a comma follows it" $
      reportErr (inplaceReport Colors [("A.hs", missing "Criterion.Main")])
        `shouldSatisfy` any (T.isInfixOf "\ESC[1mCriterion.Main\ESC[0m,")

    it "colors the cross red" $
      reportErr (inplaceReport Colors [("A.hs", failure)])
        `shouldSatisfy` any (T.isInfixOf "[\ESC[31m✗\ESC[0m]")

  describe "breaking text to fit" $ do
    it "keeps every line within the room given" $
      fmap T.length (wrapTo 20 (T.replicate 40 "word ")) `shouldSatisfy` all (<= 20)

    it "breaks at spaces and nowhere else" $
      wrapTo 12 "one two three four" `shouldBe` ["one two", "three four"]

    it "gives a word too long for the room a line of its own" $
      wrapTo 8 "a supercalifragilistic b"
        `shouldBe` ["a", "supercalifragilistic", "b"]

    it "keeps a line break that was already there" $
      wrapTo 40 "first thing\nsecond thing"
        `shouldBe` ["first thing", "second thing"]

    it "has nothing to say about nothing" $
      wrapTo 20 "" `shouldBe` []

    it "measures what a reader will see, not what the text holds" $ do
      visibleLength "abc" `shouldBe` 3
      visibleLength "\ESC[31mabc\ESC[0m" `shouldBe` 3
      visibleLength "\ESC[1m\ESC[36mab\ESC[0mc" `shouldBe` 3

    it "breaks a colored line exactly where it breaks the bare one" $ do
      let bare = "alpha beta gamma delta epsilon zeta eta theta"
          lit = T.replace "gamma" "\ESC[36mgamma\ESC[0m" bare
      fmap visibleLength (wrapTo 20 lit) `shouldBe` fmap T.length (wrapTo 20 bare)

  describe "how wide anything gets" $ do
    let sprawling =
          [ ("some/quite/deeply/nested/directory/Module.hs", decline),
            ("another/quite/deeply/nested/one/Module.hs", failure)
          ]
    it "never prints a line wider than it allows itself" $
      fmap T.length (reportErr (inplaceReport Plain sprawling))
        `shouldSatisfy` all (<= lineWidth)

    it "does the same when the reason is enormous" $
      fmap T.length (reportErr (inplaceReport Plain [("A.hs", Failed (Unreadable "A.hs" (T.replicate 40 "and more words ")))]))
        `shouldSatisfy` all (<= lineWidth)

    it "wraps what it says when it gives up, too" $
      fmap T.length (noted Plain ("✗", Bad) (T.replicate 40 "and more words "))
        `shouldSatisfy` all (<= lineWidth)

  describe "reading a file" $ do
    it "reads it as UTF-8" $
      withBytes (T.encodeUtf8 "-- λ über ✓\n") $ \path ->
        readAsUtf8 path `shouldReturn` Right "-- λ über ✓\n"

    it "reads it as UTF-8 whatever the machine's locale is" $
      withBytes (BS.pack [0xC3, 0xA9, 0x0A]) $ \path ->
        readAsUtf8 path `shouldReturn` Right "é\n"

    it "says so when it is not UTF-8 at all" $
      withBytes (BS.pack [0x6D, 0xFF, 0xFE, 0x0A]) $ \path ->
        readAsUtf8 path `shouldReturn` Left "it is not valid UTF-8"

    it "says so when it is not there" $
      withSystemTempDirectory "tilia-run" $ \directory ->
        (isLeft <$> readAsUtf8 (directory </> "gone.hs"))
          `shouldReturn` True

    it "hands over the line endings it found, rather than the platform's" $
      withBytes "module A where\r\n" $ \path ->
        readAsUtf8 path `shouldReturn` Right "module A where\r\n"

  describe "what would be written back" $ do
    it "is unchanged when the formatter gave back what was there" $
      differs (formattingOutcome "module A where\n" "module A where\n")
        `shouldBe` False

    it "is unchanged when a file with Windows endings formats to itself" $
      differs (formattingOutcome "module A where\r\n" "module A where\n")
        `shouldBe` False

    it "puts the file's own endings back on what it writes" $
      written (formattingOutcome "module A where\r\n" "module B where\n")
        `shouldBe` Just "module B where\r\n"

    it "leaves a file that ends its lines with newlines alone" $
      written (formattingOutcome "module A where\n" "module B where\n")
        `shouldBe` Just "module B where\n"

    it "takes the endings from the first line, over a file of many" $
      written (formattingOutcome "a\r\nb\r\nc\r\n" "a\nb\nd\n")
        `shouldBe` Just "a\r\nb\r\nd\r\n"

    it "counts a file whose endings disagree as one that would change" $
      differs (formattingOutcome "a\r\nb\nc\r\n" "a\nb\nc\n") `shouldBe` True

    it "says as much in the diff, having nothing else to show" $
      T.unlines (reportOut (checkReport Plain [("A.hs", formattingOutcome "a\r\nb\nc\r\n" "a\nb\nc\n")]))
        `shouldSatisfy` T.isInfixOf "differ only in how they end their lines"

    it "leaves a carriage return that is not a line ending where it is" $
      written (formattingOutcome "x = \"a\\\r b\"\r\n" "y = \"a\\\r b\"\n")
        `shouldBe` Just "y = \"a\\\r b\"\r\n"

  describe "putting a file back" $ do
    it "writes one that changed" $
      withSource "module A where\n" $ \path -> do
        writeBack (path, Changed "module A where\n" "module B where\n")
        T.readFile path `shouldReturn` "module B where\n"

    it "writes it as UTF-8, and as the bytes it was given" $
      withSource "module A where\n" $ \path -> do
        writeBack (path, Changed "module A where\n" "-- ✓ über\r\n")
        BS.readFile path `shouldReturn` T.encodeUtf8 "-- ✓ über\r\n"

    it "takes back exactly what it read, over a whole round trip" $
      withBytes (T.encodeUtf8 "-- ü\r\nmodule A where\r\n") $ \path -> do
        Right asItIs <- readAsUtf8 path
        writeBack (path, formattingOutcome asItIs "-- ü\nmodule B where\n")
        BS.readFile path `shouldReturn` T.encodeUtf8 "-- ü\r\nmodule B where\r\n"

    it "leaves one that did not alone, down to its modification time" $
      untouched Unchanged

    it "leaves one it would not format alone" $
      untouched decline

    it "leaves one it could not format alone" $
      untouched failure

  describe "running over many files at once" $ do
    it "answers in the order it was asked, not the order it finished" $ do
      answers <- inParallel (\n -> threadDelay (1000 * (40 - n)) >> pure n) [1 .. 20 :: Int]
      answers `shouldBe` [1 .. 20]

    it "runs every one of them exactly once" $ do
      seen <- newIORef (0 :: Int)
      _ <- inParallel (\_ -> atomicModifyIORef' seen (\n -> (n + 1, ()))) [1 .. 500 :: Int]
      readIORef seen `shouldReturn` 500

    it "copes with having nothing to do" $
      inParallel pure ([] :: [Int]) `shouldReturn` []

    it "really does run them at once" $ do
      capabilities <- getNumCapabilities
      let rounds = ceiling (20 / fromIntegral capabilities :: Double) :: Int
      started <- getMonotonicTime
      _ <- inParallel (\_ -> threadDelay 100000) [1 .. 20 :: Int]
      finished <- getMonotonicTime
      (finished - started) `shouldSatisfy` (< fromIntegral rounds * 0.1 + 0.4)

----------------------------------------------------------------------------
-- Helpers

unreadable, unclaimed :: FormatError
unreadable = Unreadable "A.hs" "gone"
unclaimed = NoPackage "B.hs" NoPackageFile

-- | A file we would not touch, and two we could not.
decline, failure, otherFailure :: Outcome
decline = Declined (PositionPragmas "A.hs")
failure = Failed unreadable
otherFailure = Failed unclaimed

-- | A file with the given contents, in a directory of its own.
withSource :: Text -> (FilePath -> IO a) -> IO a
withSource contents act =
  withSystemTempDirectory "tilia-run" $ \directory -> do
    let path = directory </> "A.hs"
    T.writeFile path contents
    act path

-- | The same, for a file that has to hold exactly these bytes.
withBytes :: BS.ByteString -> (FilePath -> IO a) -> IO a
withBytes contents act =
  withSystemTempDirectory "tilia-run" $ \directory -> do
    let path = directory </> "A.hs"
    BS.writeFile path contents
    act path

-- | What writing this outcome back would put in the file.
written :: Outcome -> Maybe Text
written = \case
  Changed _ wouldBe -> Just wouldBe
  _ -> Nothing

-- | Writing this outcome back should do nothing whatsoever.
untouched :: Outcome -> Expectation
untouched outcome =
  withSource "module A where\n" $ \path -> do
    stamped <- getModificationTime path
    threadDelay 10000
    writeBack (path, outcome)
    stampedAgain <- getModificationTime path
    stampedAgain `shouldBe` stamped
    T.readFile path `shouldReturn` "module A where\n"

-- | Is this the first line of an explanation rather than a continuation?
opensAReason :: Text -> Bool
opensAReason line = T.isPrefixOf "    " line && not (T.isPrefixOf "     " line)

-- | A failure that names the file it is about, as the real ones do.
failureAbout :: FilePath -> Outcome
failureAbout path = Failed (Unreadable path "no such file")

-- | A case about one operator whose fixity could not be settled.
about :: Text -> Outcome
about op = Declined (UnknownFixity "A.hs" [((Nothing, OpName op), Ambiguous)])

-- | A case about an operator whose fixity is in a module we could not read.
missing :: Text -> Outcome
missing modName = missingIn [modName]

-- | The same, where more than one module could have declared it.
missingIn :: [Text] -> Outcome
missingIn = missingThrough . fmap (ModuleChain . (:| []))

-- | Two operators with the one answer between them, which is said once.
missingTwice :: Text -> Outcome
missingTwice = missingAll ["<|>", "<+>"]

-- | Two operators with an answer each, so neither is a plurality.
twoAnswers :: Outcome
twoAnswers =
  Declined
    ( UnknownFixity
        "A.hs"
        [ ((Nothing, OpName "<|>"), NotRead (ModuleChain ("Criterion.Main" :| []) :| [])),
          ((Nothing, OpName "<+>"), Ambiguous)
        ]
    )

-- | Any number of them, all with the one answer between them.
missingAll :: [Text] -> Text -> Outcome
missingAll ops modName =
  Declined
    ( UnknownFixity
        "A.hs"
        [((Nothing, OpName op), NotRead names) | op <- ops]
    )
  where
    names = ModuleChain (modName :| []) :| []

-- | The same again, where each import is given down to the module that
-- actually stopped the reading.
missingThrough :: [ModuleChain] -> Outcome
missingThrough chains =
  Declined (UnknownFixity "A.hs" [((Nothing, OpName "<|>"), NotRead names)])
  where
    names = case chains of
      [] -> error "an operator has to be missing from somewhere"
      c : cs -> c :| cs

-- | A report as one piece of text, with the breaks it was wrapped at undone.
flattened :: Report -> Text
flattened = T.unwords . fmap T.strip . reportErr
