{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Measure what formatting a fixed sample of modules costs, and compare
-- what it allocates and the instructions it retires with the record kept
-- in the repository.
module Main (main) where

import Control.Exception (SomeException, try)
import Control.Monad (join, mfilter, unless)
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (for_)
import Data.Int (Int64)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for)
import Data.Version (showVersion)
import Data.Word (Word64)
import Options.Applicative
  ( Parser,
    ParserInfo,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    maybeReader,
    metavar,
    option,
    optional,
    progDesc,
    showDefault,
    strOption,
    value,
  )
import System.Environment (lookupEnv)
import System.Exit (die, exitFailure)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.Info (fullCompilerVersion)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Tilia.Bench.Cases
import Tilia.Bench.Fixity
import Tilia.Bench.Measure
import Tilia.Bench.Record
import Tilia.Corpus (hackagePackages, obtain)

-- | How the benchmarks were asked to run.
data Options = Options
  { -- | How many runs to measure after the one that warms up.
    optRuns :: Int,
    -- | Run only the benchmarks whose module names hold this.
    optMatch :: Maybe Text,
    -- | How far one benchmark's allocations or instructions may move from
    -- the record before it is out of date, as a fraction.
    optTolerance :: Double,
    -- | How far the allocations or instructions of all the benchmarks of a
    -- stage may move, as a fraction.
    optTotalTolerance :: Double,
    -- | Where to save what was measured.
    optSave :: Maybe FilePath,
    -- | What an earlier run saved, to compare this one with.
    optBaseline :: Maybe FilePath
  }

-- | A benchmark, by stage and module.
type Key = (Text, Text)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  options <- execParser optionsInfo
  accepting <- (== Just "1") <$> lookupEnv "TILIA_BENCH_ACCEPT"
  examples <- obtain hackagePackages >>= either (die . T.unpack) pure
  sampled <- corpusSubjects examples >>= either (die . T.unpack) pure
  let matching name = maybe True (`T.isInfixOf` name) (optMatch options)
      subjects =
        [s | s <- sampled <> syntheticSubjects, matching (subjectName s)]
      partial = isJust (optMatch options)
  counter <- openCounter
  processor <- (<* counter) <$> processorName
  written <- readRecord recordPath
  baseline <- traverse readBaseline (optBaseline options)
  let measuring stage s work input = do
        (result, m) <-
          measure counter (optRuns options) work (maybe 0 T.length) input
        let k = (stageName stage, subjectName s)
            shown = comparable processor (recordInterfaces written) written
        T.putStrLn (lineFor (Just shown) baseline k m)
        pure (result, (k, m))
  formatted <- fmap concat . for subjects $ \s -> do
    (out, formatting) <-
      measuring Format s (subjectFormat s) (subjectSource s)
    checking <- case (subjectCheck s, out) of
      (Just check, Just printed) ->
        pure . snd <$> measuring Check s check printed
      _ -> pure []
    pure (formatting : checking)
  ended <- withFixityBenchmarks $ \(resolving, interfaces) -> do
    let record = comparable processor interfaces written
        decoding = recordInterfaces written == interfaces
        checked (stage, _) = decoding || stage /= stageName Decode
    others <- for [b | b <- resolving, matching (benchmarkName b)] $ \b -> do
      (_, m) <- measureIO counter (optRuns options) (benchmarkRun b) id
      let k = (stageName (benchmarkStage b), benchmarkName b)
          shown = if checked k then Just record else Nothing
      T.putStrLn (lineFor shown baseline k m)
      pure (k, m)
    let measured = formatted <> others
    for_ (optSave options) (`writeBaseline` measured)
    totalled record measured
    for_ baseline (summarize measured)
    if accepting
      then do
        let entryOf m =
              Entry
                (measuredAllocated m)
                (measuredCopied m)
                (measuredInstructions m <* processor)
        writeRecord
          recordPath
          Record
            { recordCompiler = compiler,
              recordProcessor = fromMaybe "none" processor,
              recordInterfaces = interfaces,
              recordEntries =
                Map.union
                  (Map.fromList [(k, entryOf m) | (k, m) <- measured])
                  (if partial then recordEntries record else Map.empty)
            }
        putStrLn ("Wrote " <> recordPath <> ".")
      else do
        for_ (unchecked processor record) $ \reason ->
          T.putStrLn ("\nInstructions are not checked: " <> reason <> ".")
        unless decoding $
          putStrLn
            "\nDecoding is not checked: the record decoded other interfaces."
        let problems =
              verdict
                options
                record
                partial
                [x | x@(k, _) <- measured, checked k]
        unless (null problems) $ do
          putStrLn ""
          for_ problems T.putStrLn
          putStrLn ""
          putStrLn "If that is the intended change, regenerate the record:"
          putStrLn "    TILIA_BENCH_ACCEPT=1 cabal bench"
          exitFailure
  either (die . T.unpack) pure ended

-- | Where the record is kept, relative to the package.
recordPath :: FilePath
recordPath = "bench/bench.record"

-- | The compiler running the benchmarks.
compiler :: Text
compiler = T.pack ("ghc " <> showVersion fullCompilerVersion)

-- | The processor running the benchmarks, as Linux names it.
processorName :: IO (Maybe Text)
processorName =
  try (T.readFile "/proc/cpuinfo") >>= \case
    Left (_ :: SomeException) -> pure Nothing
    Right cpus ->
      pure $
        listToMaybe
          [ T.strip (T.drop 1 said)
          | (key, said) <- T.breakOn ":" <$> T.lines cpus,
            T.strip key == "model name"
          ]

-- | Forget what a record says that does not hold here: the instructions it
-- counted on another processor than this one, and what decoding other
-- interfaces than these cost.
comparable :: Maybe Text -> Text -> Record -> Record
comparable processor interfaces record =
  record{recordEntries = Map.mapMaybeWithKey kept (recordEntries record)}
  where
    kept (stage, _) e
      | stage == stageName Decode,
        recordInterfaces record /= interfaces =
          Nothing
      | processor == Just (recordProcessor record) = Just e
      | otherwise = Just e{entryInstructions = Nothing}

-- | Why the instructions are not checked against the record, where they
-- are not.
unchecked :: Maybe Text -> Record -> Maybe Text
unchecked processor record
  | Nothing <- processor = Just "they cannot be counted here"
  | recordProcessor record == "none" = Just "the record has none"
  | processor /= Just (recordProcessor record) =
      Just ("the record counted them on " <> recordProcessor record)
  | otherwise = Nothing

-- | What the benchmarks can be asked to do.
optionsInfo :: ParserInfo Options
optionsInfo =
  info (helper <*> optionsParser) . mconcat $
    [ fullDesc,
      progDesc "Measure what Tilia costs and check it against the record"
    ]

-- | The options of the benchmarks.
optionsParser :: Parser Options
optionsParser =
  Options
    <$> (option positive . mconcat)
      [ long "runs",
        metavar "N",
        value 1,
        showDefault,
        help "How many runs to measure after the one that warms up"
      ]
    <*> (optional . strOption . mconcat)
      [ long "match",
        metavar "TEXT",
        help "Run only the benchmarks whose names hold TEXT"
      ]
    <*> (option percent . mconcat)
      [ long "tolerance",
        metavar "PERCENT",
        value 0.005,
        help "How far one benchmark may move from the record (default: 0.5)"
      ]
    <*> (option percent . mconcat)
      [ long "total-tolerance",
        metavar "PERCENT",
        value 0.0005,
        help "How far the benchmarks of a stage may move (default: 0.05)"
      ]
    <*> (optional . strOption . mconcat)
      [ long "save",
        metavar "FILE",
        help "Save the time and instructions of every benchmark to FILE"
      ]
    <*> (optional . strOption . mconcat)
      [ long "baseline",
        metavar "FILE",
        help "Compare with what an earlier run saved to FILE"
      ]
  where
    positive = maybeReader (mfilter (> 0) . readMaybe)
    percent = maybeReader (fmap (/ 100) . mfilter (>= 0) . readMaybe)

-- | One benchmark's line of the report: its time, against the baseline
-- where there is one, and what it allocated and the instructions it
-- retired, against the record where it is checked against one.
lineFor :: Maybe Record -> Maybe Baseline -> Key -> Measurement -> Text
lineFor record baseline k@(stage, name) m =
  T.pack $
    printf
      "%-7s %8.3f s %-9s %10.1f MB %-11s %s %-9s %s%s"
      stage
      (seconds (measuredTime m))
      (maybe "" ((`against` measuredTime m) . fst) (Map.lookup k =<< baseline))
      (megabytes (measuredAllocated m))
      (maybe "(unchecked)" (maybe "(new)" (changeOf allocations)) entry)
      (instructionsColumn (measuredInstructions m))
      (maybe "" (changeOf instructions) (join entry))
      name
      differing
  where
    entry = Map.lookup k . recordEntries <$> record
    changeOf cost e = maybe "" (uncurry against) (summed cost [(e, m)])
    differing :: String
    differing
      | measuredSpread m > 0.001 =
          printf " (runs differ by %.2f%%)" (100 * measuredSpread m)
      | otherwise = ""

-- | A cost the record keeps for every benchmark.
data Cost = Cost
  { -- | The verb that says how it moved.
    costVerb :: Text,
    -- | The noun that follows how far it moved.
    costNoun :: Text,
    -- | What the record says it is.
    costRecorded :: Entry -> Maybe Word64,
    -- | What was measured.
    costMeasured :: Measurement -> Maybe Word64
  }

-- | The bytes allocated.
allocations :: Cost
allocations =
  Cost "allocates" "" (Just . entryAllocated) (Just . measuredAllocated)

-- | The bytes the garbage collector copied.
copying :: Cost
copying = Cost "has copied" "" (Just . entryCopied) (Just . measuredCopied)

-- | The instructions retired, where they were counted.
instructions :: Cost
instructions =
  Cost "retires" " instructions" entryInstructions measuredInstructions

-- | What the record says and what was measured of a cost, summed over the
-- benchmarks that have it on both sides.
summed :: Cost -> [(Entry, Measurement)] -> Maybe (Word64, Word64)
summed cost pairs =
  case [ (old, new)
       | (e, m) <- pairs,
         Just old <- [costRecorded cost e],
         Just new <- [costMeasured cost m]
       ] of
    [] -> Nothing
    both -> Just (sum (fmap fst both), sum (fmap snd both))

-- | What is wrong with the record, given what was measured: one line for
-- every benchmark it is out of date for, and for every stage whose
-- benchmarks together cost more or less than it says.
verdict :: Options -> Record -> Bool -> [(Key, Measurement)] -> [Text]
verdict options record partial measured
  | recordCompiler record /= compiler =
      [ "The record was made with "
          <> (if T.null made then "no compiler" else made)
          <> " and these benchmarks were built with "
          <> compiler
          <> ", which allocates differently."
      ]
  | otherwise =
      concatMap judged measured
        <> [ "In the record but not measured: " <> stage <> " " <> name
           | not partial,
             k@(stage, name) <- Map.keys (recordEntries record),
             k `notElem` fmap fst measured
           ]
        <> [ moving cost ("all of " <> stage) old new
           | stage <- stages measured,
             (cost, tolerance) <-
               [ (allocations, optTotalTolerance options),
                 (copying, copiedTolerance),
                 (instructions, optTotalTolerance options)
               ],
             Just (old, new) <- [summed cost (known record stage measured)],
             moved tolerance old new
           ]
  where
    made = recordCompiler record
    judged (k@(stage, name), m) = case Map.lookup k (recordEntries record) of
      Nothing -> ["Not in the record: " <> stage <> " " <> name]
      Just e ->
        [ moving cost (stage <> " " <> name) old new
        | cost <- [allocations, instructions],
          Just (old, new) <- [summed cost [(e, m)]],
          moved (optTolerance options) old new
        ]
    moved tolerance old new =
      abs (fromIntegral new - fromIntegral old)
        > tolerance * (fromIntegral old :: Double)
    moving :: Cost -> Text -> Word64 -> Word64 -> Text
    moving cost subject old new =
      T.pack $
        printf
          "%s %s %.2f%% %s%s than the record says"
          subject
          (costVerb cost)
          (abs (change (fromIntegral old) (fromIntegral new)))
          (if new > old then "more" else "less" :: String)
          (costNoun cost)

-- | How far the bytes copied by all the benchmarks of a stage may move from
-- the record before it is out of date, as a fraction: the copying one
-- benchmark causes moves with where the collections fall, which a change
-- to what it allocates shifts, and only the sum of them is steady.
copiedTolerance :: Double
copiedTolerance = 0.01

-- | Say what the benchmarks of each stage took, allocated and retired
-- together, and how that compares with the record.
totalled :: Record -> [(Key, Measurement)] -> IO ()
totalled record measured = do
  putStrLn ""
  for_ (stages measured) $ \stage -> do
    let ms = [m | ((s, _), m) <- measured, s == stage]
        sumOf f = sum (fmap f ms)
        changeOf cost =
          maybe "" (uncurry against) (summed cost (known record stage measured))
    printf
      ( "%-7s %8.3f s %9s %10.1f MB %-11s %s %-9s "
          <> "copied %.1f MB %s, %d benchmarks\n"
      )
      stage
      (seconds (sumOf measuredTime))
      ("" :: String)
      (megabytes (sumOf measuredAllocated))
      (changeOf allocations)
      (instructionsColumn (sum <$> traverse measuredInstructions ms))
      (changeOf instructions)
      (megabytes (sumOf measuredCopied))
      (changeOf copying)
      (length ms)

-- | The stages measured, in the order they were.
stages :: [(Key, Measurement)] -> [Text]
stages = nubOrd . fmap (fst . fst)

-- | What the record and the measurements say about each benchmark of a
-- stage that both have.
known :: Record -> Text -> [(Key, Measurement)] -> [(Entry, Measurement)]
known record stage measured =
  [ (e, m)
  | (k@(s, _), m) <- measured,
    s == stage,
    Just e <- [Map.lookup k (recordEntries record)]
  ]

-- | What an earlier run measured: the time of every benchmark and the
-- instructions it retired, where they were counted.
type Baseline = Map Key (Int64, Maybe Word64)

-- | Save what was measured.
writeBaseline :: FilePath -> [(Key, Measurement)] -> IO ()
writeBaseline path measured =
  T.writeFile path . T.unlines $
    [ T.unwords
        [ stage,
          T.pack (show (measuredTime m)),
          maybe "-" (T.pack . show) (measuredInstructions m),
          name
        ]
    | ((stage, name), m) <- measured
    ]

-- | Read what 'writeBaseline' saved.
readBaseline :: FilePath -> IO Baseline
readBaseline path = do
  text <- T.readFile path
  pure $
    Map.fromList
      [ ((stage, T.unwords rest), (t, readMaybe (T.unpack retired)))
      | stage : time : retired : rest <- fmap T.words (T.lines text),
        Just t <- [readMaybe (T.unpack time)]
      ]

-- | Say how the benchmarks measured on both sides moved against the
-- baseline, all together and where they moved most: by the instructions
-- they retired where both sides counted them, by time otherwise.
summarize :: [(Key, Measurement)] -> Baseline -> IO ()
summarize measured baseline =
  unless (null both) $ do
    putStrLn ""
    printf "Against the baseline, over %d benchmarks:\n" (length both)
    printf
      "  time %.3f s -> %.3f s %s\n"
      (seconds oldTime)
      (seconds newTime)
      (oldTime `against` newTime)
    for_ counted $ \retired ->
      let old = sum (fmap fst retired)
          new = sum (fmap snd retired)
       in printf
            "  instructions %.3f G -> %.3f G %s\n"
            (billions old)
            (billions new)
            (old `against` new)
    for_ (take 5 (sortOn (Down . abs . snd) movers)) $ \((stage, name), c) ->
      printf "  (%+.2f%%) %-7s %s\n" c stage name
  where
    both =
      [ (k, ((t, i), (measuredTime m, measuredInstructions m)))
      | (k, m) <- measured,
        Just (t, i) <- [Map.lookup k baseline]
      ]
    oldTime = sum [t | (_, ((t, _), _)) <- both]
    newTime = sum [t | (_, (_, (t, _))) <- both]
    counted =
      traverse (\(_, ((_, old), (_, new))) -> (,) <$> old <*> new) both
    movers = case counted of
      Just retired ->
        zip (fmap fst both) [change' old new | (old, new) <- retired]
      Nothing ->
        [(k, change' old new) | (k, ((old, _), (new, _))) <- both]
    change' :: (Integral a) => a -> a -> Double
    change' old new = change (fromIntegral old) (fromIntegral new)

-- | How much a quantity moved from the first value to the second, as the
-- report writes it.
against :: (Integral a) => a -> a -> String
against old new =
  printf "(%+.2f%%)" (change (fromIntegral old) (fromIntegral new))

-- | How much a quantity moved, in percent.
change :: Double -> Double -> Double
change old new
  | old == 0 = 0
  | otherwise = 100 * (new - old) / old

-- | Instructions, in billions, or as much space where they were not
-- counted.
instructionsColumn :: Maybe Word64 -> String
instructionsColumn =
  maybe (replicate 15 ' ') (printf "%7.3f G instr" . billions)

-- | Nanoseconds, in seconds.
seconds :: Int64 -> Double
seconds ns = fromIntegral ns / 1e9

-- | Bytes, in megabytes.
megabytes :: Word64 -> Double
megabytes b = fromIntegral b / 1e6

-- | A count, in billions.
billions :: Word64 -> Double
billions n = fromIntegral n / 1e9
