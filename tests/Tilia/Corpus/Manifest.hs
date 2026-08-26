{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What a corpus too large to argue about example by example is expected to
-- do.
module Tilia.Corpus.Manifest
  ( -- * What an example does
    Outcome (..),
    outcomeName,

    -- * Records of it
    Manifest,
    readManifest,
    writeManifest,
    writeReport,
    accepting,
  )
where

import Control.Exception (SomeException, try)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory)

----------------------------------------------------------------------------
-- What an example does

-- | What running the formatter over one example established, coarsely enough
-- to write down and compare.
data Outcome
  = -- | Nothing was found wrong with it.
    Formatted
  | -- | Nothing was found wrong with it, and one of the things worth asking
    -- went unasked because it could not be afforded. See
    -- 'Tilia.CorpusSpec.checkCpp'.
    PartlyChecked
  | -- | The input is Haskell, and the formatter refuses to rewrite it.
    Declined
  | -- | GHC's own parser could not read it. Common in a corpus of real
    -- releases, which carry files that are Haskell templates, files written
    -- for compilers other than GHC, and files whose @CPP@ we do not expand.
    DoesNotParse
  | -- | The bytes are not UTF-8, so there was nothing to parse.
    NotUtf8
  | -- | A property does not hold. The work list.
    Broken
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | What an outcome is called in a manifest.
outcomeName :: Outcome -> Text
outcomeName = \case
  Formatted -> "formatted"
  PartlyChecked -> "partly-checked"
  Declined -> "declined"
  DoesNotParse -> "does-not-parse"
  NotUtf8 -> "not-utf8"
  Broken -> "broken"

-- | Reading one back, by the name it was written under.
outcomeNamed :: Text -> Maybe Outcome
outcomeNamed name =
  lookup name [(outcomeName o, o) | o <- [minBound .. maxBound]]

----------------------------------------------------------------------------
-- Records of it

-- | What each example of a corpus is expected to do, by name.
type Manifest = Map FilePath Outcome

-- | Read a manifest.
--
-- A missing one is an empty one rather than an error, so that a corpus added
-- to this repository before its manifest has been generated says which
-- examples it does not know about, one line each, instead of failing once
-- with a message about a file.
readManifest :: FilePath -> IO Manifest
readManifest path =
  try (T.readFile path) >>= \case
    Left (_ :: SomeException) -> pure Map.empty
    Right text -> pure (Map.fromList (concatMap entry (T.lines text)))
  where
    entry line = case T.words line of
      (what : rest)
        | not ("#" `T.isPrefixOf` what),
          Just outcome <- outcomeNamed what,
          not (null rest) ->
            [(T.unpack (T.unwords rest), outcome)]
      _ -> []

-- | Write a manifest, sorted by name so that a regeneration diff shows what
-- changed rather than what moved.
writeManifest :: FilePath -> Manifest -> IO ()
writeManifest path manifest = do
  createDirectoryIfMissing True (takeDirectory path)
  T.writeFile path (T.unlines (header <> map line (Map.toAscList manifest)))
  where
    header =
      [ "# What each example of this corpus does today, one line each.",
        "# Generated: run the test suite with TILIA_CORPUS_ACCEPT=1.",
        "# See Tilia.Corpus.Manifest for what the outcomes mean, and the",
        "# matching .report for why each example that is not `formatted` is",
        "# not.",
        ""
      ]
    line (name, outcome) =
      T.justifyLeft width ' ' (outcomeName outcome) <> T.pack name
    width = 2 + maximum (1 : map (T.length . outcomeName) (Map.elems manifest))

-- | Write the reasons beside the record.
writeReport :: FilePath -> [(FilePath, Outcome, Text)] -> IO ()
writeReport path entries = do
  createDirectoryIfMissing True (takeDirectory path)
  T.writeFile path (T.unlines (header <> concatMap section grouped))
  where
    interesting = [e | e@(_, outcome, _) <- entries, outcome /= Formatted]
    grouped =
      [ (outcome, [(name, why) | (name, o, why) <- sortOn first interesting, o == outcome])
        | outcome <- sections
      ]
    first (name, _, _) = name
    sections = [Broken, DoesNotParse, PartlyChecked, Declined, NotUtf8]
    header =
      [ "Why every example of this corpus that is not `formatted` is not.",
        "",
        "Generated beside the manifest, and compared against nothing: this",
        "file is the work list, and it is free to say as much as it likes.",
        "",
        T.pack (show (length entries)) <> " examples, "
          <> T.pack (show (length entries - length interesting))
          <> " formatted.",
        ""
      ]
    section (_, []) = []
    section (outcome, es) =
      [ T.replicate 74 "=",
        outcomeName outcome <> " (" <> T.pack (show (length es)) <> ")",
        T.replicate 74 "=",
        ""
      ]
        <> concatMap entry es
    entry (name, why) =
      [T.pack name] <> map ("    " <>) (T.lines (T.strip why)) <> [""]

-- | Is this run supposed to write the records rather than check against
-- them?
accepting :: IO Bool
accepting =
  lookupEnv "TILIA_CORPUS_ACCEPT" >>= \case
    Just s | s `notElem` ["", "0", "no", "false"] -> pure True
    _ -> pure False
