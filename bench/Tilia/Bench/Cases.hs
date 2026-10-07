{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the benchmarks of formatting measure: formatting a fixed sample of
-- the Hackage corpus and checking what that printed, and formatting modules
-- made to stress one part of the formatter each.
module Tilia.Bench.Cases
  ( Stage (..),
    stageName,
    Benchmark (..),
    Subject (..),
    corpusSubjects,
    syntheticSubjects,
  )
where

import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Traversable (for)
import GHC.LanguageExtensions.Type (Extension)
import Tilia.Corpus (Example (..))
import Tilia.Cpp (everyBranch, formatWithCpp, usesCpp)
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Format (rewritten)
import Tilia.Parser (parseModule, parserConfigFor)
import Tilia.Pragma (effectiveExtensions)
import Tilia.Render (defaultRenderConfig, renderModule)
import Tilia.TestConfig (exampleRenderConfig)

-- | What part of a run a benchmark measures.
data Stage
  = -- | Formatting a module.
    Format
  | -- | Checking that what formatting printed is the program it was given,
    -- as @--check-ast@ does.
    Check
  | -- | Reading what the modules of a package establish out of its tarball.
    Resolve
  | -- | The same, out of a cache that an earlier reading filled.
    Recall
  | -- | Decoding the interfaces of a package.
    Decode

-- | A stage, as the record and the report spell it.
stageName :: Stage -> Text
stageName = \case
  Format -> "format"
  Check -> "check"
  Resolve -> "resolve"
  Recall -> "recall"
  Decode -> "decode"

-- | A benchmark of something other than formatting a module.
data Benchmark = Benchmark
  { -- | What it measures.
    benchmarkStage :: Stage,
    -- | What it measures that on.
    benchmarkName :: Text,
    -- | Set one run up, giving the work, which says how much it computed.
    benchmarkRun :: IO (IO Int)
  }

-- | A module the benchmarks format, and check where they check it.
data Subject = Subject
  { -- | Its name in the corpus, or the stress it makes.
    subjectName :: Text,
    -- | The module.
    subjectSource :: Text,
    -- | Format it, or 'Nothing' where it is declined.
    subjectFormat :: Text -> Maybe Text,
    -- | Check what formatting it printed, giving what changed, where it is
    -- checked.
    subjectCheck :: Maybe (Text -> Maybe Text)
  }

-- | The corpus modules formatted and checked, by the name the corpus gives
-- them: middling ones from many packages, a few with conditionals among
-- them.
checked :: [FilePath]
checked =
  [ "haxl-2.5.1.1/Haxl/Core/Monad.hs",
    "vector-0.13.2.0/src/Data/Vector/Generic.hs",
    "aeson-2.3.1.0/src/Data/Aeson/KeyMap.hs",
    "pandoc-3.10.2/src/Text/Pandoc/Class/PandocPure.hs",
    "lifted-base-0.2.3.12/Control/Exception/Lifted.hs",
    "text-2.1.4/tests/Tests/Properties/Folds.hs",
    "ShellCheck-0.11.0/src/ShellCheck/Analytics.hs",
    "Agda-2.8.0/src/full/Agda/Syntax/Translation/ConcreteToAbstract.hs",
    "idris-1.3.4/src/Idris/Elab/Term.hs",
    "pandoc-3.10.2/src/Text/Pandoc/Writers/Powerpoint/Output.hs",
    "purescript-0.15.15/src/Language/PureScript/Errors.hs",
    "pandoc-3.10.2/src/Text/Pandoc/Readers/Markdown.hs",
    "esqueleto-3.6.0.3/test/PostgreSQL/Test.hs",
    "unpacked-containers-0/src/Map/Internal.hs",
    "purescript-0.15.15/src/Language/PureScript/TypeChecker/Types.hs",
    "distributed-process-0.7.8/src/Control/Distributed/Process/Node.hs",
    "servant-server-0.20.3.0/src/Servant/Server/Internal.hs",
    "brittany-0.14.0.2/source/library/Language/Haskell/Brittany/Internal\
    \/Transformations/Alt.hs",
    "xmonad-0.18.1/src/XMonad/Operations.hs",
    "swagger2-2.9.1/src/Data/Swagger/Internal.hs",
    "time-1.16.0.1/test/main/Test/Format/ParseTime.hs",
    "optparse-applicative-0.19.0.0/tests/test.hs",
    "blaze-html-0.9.2.0/src/Text/Blaze/Html5/Attributes.hs",
    "tls-2.4.3/Network/TLS/Extension.hs",
    "recursion-schemes-5.2.3/src/Data/Functor/Foldable.hs",
    "megaparsec-9.8.1/Text/Megaparsec/Internal.hs"
  ]

-- | The corpus modules only formatted: the largest ones, whose checks would
-- take most of the time.
formattedOnly :: [FilePath]
formattedOnly =
  [ "QuickCheck-2.18.0.0/src/Test/QuickCheck/Arbitrary.hs",
    "unordered-containers-0.2.21/Data/HashMap/Internal.hs",
    "lens-5.3.6/src/Control/Lens/Wrapped.hs",
    "text-2.1.4/src/Data/Text/Internal/Fusion/CaseMapping.hs"
  ]

-- | The modules of the corpus measured, or which one the corpus does not
-- have.
corpusSubjects :: [Example] -> IO (Either Text [Subject])
corpusSubjects examples =
  sequence
    <$> for
      (fmap (,True) checked <> fmap (,False) formattedOnly)
      (uncurry load)
  where
    byName = Map.fromList [(exampleName e, e) | e <- examples]
    load name isChecked = case Map.lookup name byName of
      Nothing -> pure (Left ("the corpus has no " <> T.pack name))
      Just example -> do
        bytes <- BS.readFile (exampleInput example)
        pure $ case decodeUtf8' bytes of
          Left _ -> Left (T.pack name <> " is not valid UTF-8")
          Right source ->
            Right $
              subjectOf
                (exampleExtensions example)
                isChecked
                (T.pack name)
                source

-- | Modules made to stress one part of the formatter each.
syntheticSubjects :: [Subject]
syntheticSubjects =
  [ subjectOf [] False "synthetic/comments" commented,
    subjectOf [] False "synthetic/imports" imported,
    subjectOf [] False "synthetic/conditionals" conditional,
    subjectOf [] False "synthetic/nesting" nested
  ]
  where
    commented =
      T.unlines $
        ["module Commented where", ""]
          <> concat
            [ [ "-- | The function number " <> n <> ", which is documented.",
                "f" <> n <> " :: Int -> Int -- the type",
                "f" <> n <> " x =",
                "  {- a block comment -} x + " <> n <> " -- an end-of-line one",
                ""
              ]
            | n <- numbers 1500
            ]
    imported =
      T.unlines $
        ["module Imported where", ""]
          <> [ "import M" <> m <> " (f" <> n <> ", T" <> n <> " (..))"
             | n <- numbers 4,
               m <- numbers 250
             ]
          <> ["", "x :: Int", "x = 1"]
    conditional =
      T.unlines $
        ["{-# LANGUAGE CPP #-}", "module Conditional where", ""]
          <> concat
            [ [ "#ifdef FLAG" <> n,
                "g" <> n <> " :: Int",
                "g" <> n <> " = " <> n,
                "#else",
                "g" <> n <> " :: Integer",
                "g" <> n <> " = " <> n <> " + 1",
                "#endif",
                ""
              ]
            | n <- numbers 24
            ]
    nested =
      T.unlines
        [ "module Nested where",
          "",
          "deep :: Int -> Int",
          "deep x =",
          "  " <> foldr deeper "x" (numbers 40),
          "",
          "wide :: [Int]",
          "wide = [" <> T.intercalate ", " (numbers 5000) <> "]"
        ]
    deeper n e =
      "case x of { " <> n <> " -> (" <> e <> "); _ -> x + " <> n <> " }"
    numbers k = fmap (T.pack . show) [1 :: Int .. k]

-- | A module to format, and to check what that printed where it is
-- checked.
subjectOf :: [Extension] -> Bool -> Text -> Text -> Subject
subjectOf package isChecked name source =
  Subject
    { subjectName = name,
      subjectSource = source,
      subjectFormat = formatted package path,
      subjectCheck =
        if isChecked
          then Just (fst . rewritten parser cpp path (source, tree))
          else Nothing
    }
  where
    path = T.unpack name
    parser = parserConfigFor package
    cpp = usesCpp (effectiveExtensions package source) source
    tree
      | cpp = Nothing
      | otherwise = either (const Nothing) Just (parseModule parser path source)

-- | What formatting a module prints, as the corpus does it, or 'Nothing'
-- where it is declined.
formatted :: [Extension] -> FilePath -> Text -> Maybe Text
formatted package path source
  | usesCpp (effectiveExtensions package source) source =
      either (const Nothing) Just (formatWithCpp parser configured path source)
  | otherwise = case parseModule parser path source of
      Left _ -> Nothing
      Right p ->
        Just
          ( printDoc
              defaultRenderOptions
              (renderModule (exampleRenderConfig package source (pure p)) p)
          )
  where
    parser = parserConfigFor package
    configured =
      maybe
        defaultRenderConfig
        (exampleRenderConfig package source)
        (everyBranch parser path source)
