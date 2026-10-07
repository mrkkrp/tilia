{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | What the benchmarks cost when they were last recorded, kept in the
-- repository so that a change to it shows in review like any other.
module Tilia.Bench.Record
  ( Record (..),
    Entry (..),
    readRecord,
    writeRecord,
  )
where

import Control.Exception (SomeException, try)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Word (Word64)
import Text.Read (readMaybe)

-- | The bytes each benchmark allocated and the instructions it retired, by
-- stage and module.
data Record = Record
  { -- | The compiler the record was made with, which what is allocated
    -- depends on.
    recordCompiler :: Text,
    -- | The processor the instructions were counted on, which how many
    -- there are depends on, or @none@.
    recordProcessor :: Text,
    -- | A digest of the names and sizes of the interfaces decoded, which
    -- what decoding them costs depends on.
    recordInterfaces :: Text,
    -- | One entry for every benchmark.
    recordEntries :: Map (Text, Text) Entry
  }

-- | What one benchmark cost.
data Entry = Entry
  { -- | The bytes it allocated.
    entryAllocated :: !Word64,
    -- | The instructions it retired, where they were counted.
    entryInstructions :: !(Maybe Word64)
  }

-- | Read a record. A missing one is an empty one, so that benchmarks added
-- before it is regenerated each say they are not in it.
readRecord :: FilePath -> IO Record
readRecord path =
  try (T.readFile path) >>= \case
    Left (_ :: SomeException) -> pure (Record "" "none" "" Map.empty)
    Right text ->
      pure
        Record
          { recordCompiler = field "compiler " text,
            recordProcessor = field "processor " text,
            recordInterfaces = field "interfaces " text,
            recordEntries = Map.fromList (concatMap entry (T.lines text))
          }
  where
    field key = mconcat . mapMaybe (T.stripPrefix key) . T.lines
    entry line = case T.words line of
      [stage, allocated, instructions, name]
        | Just a <- readMaybe (T.unpack allocated) ->
            [((stage, name), Entry a (readMaybe (T.unpack instructions)))]
      _ -> []

-- | Write a record, sorted by stage and module so that a regeneration diff
-- shows what changed rather than what moved.
writeRecord :: FilePath -> Record -> IO ()
writeRecord path record =
  T.writeFile path . T.unlines $
    [ "# What each benchmark costs, as far as that does not depend on how",
      "# busy the machine is. `cabal bench` checks it, and",
      "# `TILIA_BENCH_ACCEPT=1 cabal bench` writes it again.",
      "#",
      "# compiler    The compiler the benchmarks were built with, which what",
      "#             they allocate depends on.",
      "# processor   The processor the instructions were counted on, which",
      "#             how many there are depends on, or none.",
      "# interfaces  A digest of the names and sizes of the interfaces the",
      "#             decode stage reads, which what decoding them costs",
      "#             depends on.",
      "#",
      "# Every other line is a benchmark:",
      "#",
      "# stage         What it measures: formatting a module (format),",
      "#               checking what that printed as --check-ast does",
      "#               (check), reading what every module of a package",
      "#               establishes out of its tarball with no cache",
      "#               (resolve), the same out of a cache an earlier reading",
      "#               filled (recall), or decoding every interface of a",
      "#               package the compiler ships with (decode).",
      "# allocated     The bytes it allocates.",
      "# instructions  The instructions it retires in user space, or - where",
      "#               they were not counted.",
      "# benchmark     The module or the package it measures that on.",
      "",
      "compiler " <> recordCompiler record,
      "processor " <> recordProcessor record,
      "interfaces " <> recordInterfaces record,
      "",
      row "# stage" ["allocated", "instructions"] "benchmark"
    ]
      <> fmap line (Map.toAscList (recordEntries record))
  where
    line ((stage, name), Entry allocated instructions) =
      row
        stage
        [shown allocated, maybe "-" shown instructions]
        name
    row stage costs name =
      T.justifyLeft 8 ' ' stage
        <> foldMap (T.justifyRight 14 ' ') costs
        <> "  "
        <> name
    shown = T.pack . show
