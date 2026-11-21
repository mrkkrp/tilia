{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Corpora of Haskell to run the formatter over.
module Tilia.Corpus
  ( -- * Corpora
    Corpus (..),
    Source (..),
    Reference (..),
    Expectations (..),
    Lists (..),
    vendoredExamples,
    ghcTestSuite,
    hackagePackages,

    -- * Obtaining one
    Example (..),
    answerFile,
    obtain,
    corpusCache,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Monad (forM, guard)
import Data.ByteString.Lazy qualified as BL
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf, sort, stripPrefix)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    removePathForcibly,
    renameDirectory,
    renameFile,
  )
import System.Environment (lookupEnv)
import System.FilePath (splitDirectories, takeDirectory, (</>))

import Tilia.Cabal.Package (newPackageReader, sourceExtensions)
import Tilia.Cabal.Project (ProjectRoot (..), findProjectRoot)
import Tilia.Cabal.Target
  ( describeTargetProblem,
    packageFilesOf,
    projectModules,
  )
import Tilia.Pragma (Defaults)
import Tilia.TestUtils (haskellFilesIn)
import Tilia.Utils (quietly)

----------------------------------------------------------------------------
-- Corpora

-- | Whether a corpus says what the formatted result should look like.
data Reference
  = -- | It does not, so only the properties that hold of any input can be
    -- checked.
    NoReference
  | -- | It does, in a file whose name is the input's with the given mark
    -- put before the extension, for 'alternativeSettings' as well, in that
    -- file unless they give something else, which is in a file with a
    -- prime after the mark.
    ReferenceMarked String
  deriving (Eq, Show)

-- | Where the examples of a corpus come from.
data Source
  = -- | Fetched from the network and unpacked into a cache, once per
    -- machine.
    Fetched (Url 'Https, Option 'Https) FilePath
  | -- | Hackage releases, each unpacked beside the others under one root.
    HackageReleases [String]
  | -- | Checked into this repository, so always at hand and never fetched.
    Vendored FilePath

-- | What a corpus says it expects of the formatter.
data Expectations
  = -- | Named by hand, here. Every example not named is expected to format.
    Listed Lists
  | -- | Recorded in a file, one line per example, holding what each of them
    -- does today.
    Recorded FilePath

-- | The exceptions a 'Listed' corpus makes, named in full.
data Lists = Lists
  { -- | Examples GHC's own parser cannot read, run all the same to check
    -- that it still cannot.
    expectUnreadable :: [FilePath],
    -- | Examples the formatter is supposed to refuse.
    expectDeclined :: [FilePath]
  }

-- | Where a corpus comes from and what is in it.
data Corpus = Corpus
  { -- | Used for the cache directory and in test names.
    corpusName :: String,
    -- | Where its examples come from.
    corpusSource :: Source,
    -- | Whether the corpus says what the formatted result should look like.
    corpusReference :: Reference,
    -- | What it expects the formatter to make of them.
    corpusExpectations :: Expectations,
    -- | Are these modules of the package around them?
    --
    -- True for a corpus of releases, where a module is compiled with its
    -- package's @default-extensions@, which it may not parse without, and
    -- the imports of its project's modules come after the rest.
    corpusInPackages :: Bool
  }

-- | Our own examples.
vendoredExamples :: Corpus
vendoredExamples =
  Corpus
    { corpusName = "tilia",
      corpusSource = Vendored ("corpora" </> "vendored"),
      corpusReference = ReferenceMarked "-out",
      corpusExpectations =
        Listed
          Lists
            { expectUnreadable = [],
              expectDeclined =
                [ "pragma" </> "position" </> "position-pragmas.hs",
                  "pragma" </> "position" </> "line-markers.hs",
                  "pragma" </> "position" </> "position-pragma-after-a-string-gap.hs",
                  "cpp" </> "directive" </> "unbalanced.hs",
                  "cpp" </> "directive" </> "include-in-a-quasi-quotation.hs",
                  "cpp" </> "macro" </> "macro-standing-for-a-bracket.hs",
                  "cpp" </> "macro" </> "macro-before-a-leading-comma.hs",
                  "cpp" </> "expression" </> "branch-opening-a-bracket-it-does-not-close.hs",
                  "cpp" </> "expression" </> "string-lines-joined-by-cpp.hs",
                  "cpp" </> "macro" </> "macro-for-a-context-before-a-haddock.hs",
                  "cpp" </> "directive" </> "line-marker-passed-on-by-cpp.hs",
                  "magic-comment" </> "magic-comment-inside-a-declaration.hs",
                  "cpp" </> "directive" </> "directive-left-as-written.hs",
                  "cpp" </> "settled" </> "complementary-conditionals-inside-an-expression.hs"
                ]
            },
      corpusInPackages = False
    }

-- | GHC's test suite.
ghcTestSuite :: Corpus
ghcTestSuite =
  Corpus
    { corpusName = "ghc-9.14.1-testsuite",
      corpusSource =
        Fetched
          ( https "codeload.github.com"
              /: "ghc"
              /: "ghc"
              /: "tar.gz"
              /: "refs"
              /: "tags"
              /: "ghc-9.14.1-release",
            mempty
          )
          ("testsuite" </> "tests"),
      corpusReference = NoReference,
      corpusExpectations =
        Listed
          Lists
            { expectUnreadable = ghcUnreadable,
              expectDeclined = ghcDeclined
            },
      corpusInPackages = False
    }

-- | GHC test suite files the formatter is right to refuse.
ghcDeclined :: [FilePath]
ghcDeclined =
  [ "driver" </> "T17786.hs",
    "ghci.debugger" </> "HappyTest.hs",
    "overloadedrecflds" </> "should_run" </> "overloadedlabelsrun04.hs",
    "parser" </> "should_compile" </> "ColumnPragma.hs",
    "parser" </> "should_compile" </> "T7118.hs",
    "perf" </> "compiler" </> "T20261.hs",
    "perf" </> "compiler" </> "T5631.hs",
    "programs" </> "joao-circular" </> "Funcs_Parser_Lazy.hs",
    "quasiquotation" </> "T4150.hs",
    "th" </> "T23378A.hs"
  ]

-- | Packages from Hackage.
hackagePackages :: Corpus
hackagePackages =
  Corpus
    { corpusName = "hackage",
      corpusSource = HackageReleases hackageReleases,
      corpusReference = NoReference,
      corpusExpectations =
        Recorded ("corpora" </> "hackage" </> "hackage.manifest"),
      corpusInPackages = True
    }

hackageReleases :: [String]
hackageReleases =
  [ "Agda-2.8.0",
    "HUnit-1.6.2.0",
    "QuickCheck-2.18.0.0",
    "ShellCheck-0.11.0",
    "adjunctions-4.4.4",
    "aeson-2.3.1.0",
    "ansi-terminal-1.1.5",
    "async-2.2.6",
    "attoparsec-0.14.4",
    "aws-0.25.3",
    "base64-bytestring-1.2.1.0",
    "bifunctors-5.6.3",
    "blaze-html-0.9.2.0",
    "blaze-markup-0.8.3.0",
    "brick-2.13",
    "brittany-0.14.0.2",
    "capability-0.5.0.1",
    "cassava-0.5.5.0",
    "comonad-5.0.10",
    "conduit-1.3.6.1",
    "contravariant-1.5.6",
    "criterion-1.6.5.0",
    "cryptonite-0.30",
    "dhall-1.42.3",
    "diagrams-core-1.5.1.2",
    "distributed-process-0.7.8",
    "dlist-1.0",
    "esqueleto-3.6.0.3",
    "exceptions-0.10.12",
    "fay-0.24.2.0",
    "flatparse-0.5.6.1",
    "free-5.2",
    "fsnotify-0.4.4.0",
    "hakyll-4.17.0.0",
    "hashable-1.5.1.0",
    "haxl-2.5.1.1",
    "hedgehog-1.7",
    "hledger-1.52.1",
    "hlint-3.10",
    "hspec-core-2.11.17",
    "http-client-0.7.19",
    "http-types-0.12.6",
    "idris-1.3.4",
    "intero-0.1.40",
    "leksah-0.16.2.2",
    "lens-5.3.6",
    "lifted-base-0.2.3.12",
    "megaparsec-9.8.1",
    "microlens-0.5.0.0",
    "mtl-2.3.2",
    "optics-0.4.2.1",
    "optparse-applicative-0.19.0.0",
    "pandoc-3.10.2",
    "pandoc-types-1.23.1.2",
    "parsec3-1.0.1.8",
    "parser-combinators-1.3.1",
    "persistent-2.18.1.0",
    "pipes-4.3.16",
    "postgrest-9.0.1",
    "profunctors-5.6.3",
    "purescript-0.15.15",
    "raaz-0.3.11",
    "random-1.3.1",
    "recursion-schemes-5.2.3",
    "resourcet-1.3.0",
    "retry-0.9.3.1",
    "safe-exceptions-0.1.7.4",
    "scientific-0.3.8.1",
    "scotty-0.30",
    "semigroupoids-6.0.2",
    "servant-0.20.3.0",
    "servant-server-0.20.3.0",
    "shake-0.19.9",
    "singletons-3.0.4",
    "split-0.2.5",
    "stack-9.9.9",
    "statistics-0.16.5.0",
    "stm-2.5.3.1",
    "swagger2-2.9.1",
    "tasty-1.5.4",
    "tensorflow-0.2.0.1",
    "text-2.1.4",
    "th-abstraction-0.7.2.0",
    "time-1.16.0.1",
    "tls-2.4.3",
    "transformers-0.6.3.0",
    "typed-process-0.2.13.0",
    "unliftio-0.2.25.1",
    "unordered-containers-0.2.21",
    "unpacked-containers-0",
    "uuid-types-1.0.6.1",
    "vector-0.13.2.0",
    "vector-algorithms-0.9.1.0",
    "wai-3.2.5",
    "warp-3.4.15",
    "xmonad-0.18.1",
    "yesod-core-1.7.0.0"
  ]

-- | GHC test suite files GHC's own parser cannot read.
ghcUnreadable :: [FilePath]
ghcUnreadable =
  [ "cabal" </> "sigcabal01" </> "p" </> "Map.hsig",
    "driver" </> "dynamicToo" </> "dynamicToo005" </> "A005.hsig",
    "annotations" </> "should_fail" </> "T19374b.hs",
    "annotations" </> "should_fail" </> "T19374c.hs",
    "annotations" </> "should_fail" </> "annfail13.hs",
    "arrows" </> "should_fail" </> "T2111.hs",
    "arrows" </> "should_fail" </> "arrowfail003.hs",
    "codeGen" </> "should_run" </> "T12855.hs",
    "corelint" </> "T21115.hs",
    "dependent" </> "should_fail" </> "RenamingStar.hs",
    "dependent" </> "should_fail" </> "T16326_Fail7.hs",
    "driver" </> "T16167.hs",
    "driver" </> "T16476a.hs",
    "driver" </> "T16476b.hs",
    "driver" </> "bug1677" </> "Foo.hs",
    "ffi" </> "should_fail" </> "capi_wrapper.hs",
    "ffi" </> "should_fail" </> "ccall_value.hs",
    "gadt" </> "records-fail1.hs",
    "generics" </> "Uniplate" </> "GUniplate.hs",
    "ghci.debugger" </> "mdo.hs",
    "ghci.debugger" </> "scripts" </> "TupleN.hs",
    "ghci.debugger" </> "scripts" </> "break015.hs",
    "ghci.debugger" </> "scripts" </> "dynbrk005.hs",
    "ghci" </> "prog009" </> "A3.hs",
    "ghci" </> "prog013" </> "Bad.hs",
    "ghci" </> "scripts" </> "ghci022.hs",
    "ghci" </> "scripts" </> "ghci044a.hs",
    "haddock" </> "should_compile_flag_haddock" </> "haddockA004.hs",
    "haddock" </> "should_compile_flag_haddock" </> "haddockA011.hs",
    "haddock" </> "should_compile_noflag_haddock" </> "haddockC004.hs",
    "haddock" </> "should_compile_noflag_haddock" </> "haddockC011.hs",
    "haddock" </> "should_fail_flag_haddock" </> "haddockE003.hs",
    "linear" </> "should_fail" </> "LinearNoExt.hs",
    "linear" </> "should_fail" </> "LinearNoExtU.hs",
    "linear" </> "should_fail" </> "T20083.hs",
    "mdo" </> "should_fail" </> "mdofail005.hs",
    "module" </> "Mod178_2.hs",
    "module" </> "T11432.hs",
    "module" </> "T11432a.hs",
    "module" </> "T12026.hs",
    "module" </> "mod182.hs",
    "module" </> "mod183.hs",
    "module" </> "mod69.hs",
    "module" </> "mod70.hs",
    "module" </> "mod76.hs",
    "module" </> "mod89.hs",
    "module" </> "mod98.hs",
    "parser" </> "should_compile" </> "T10582.hs",
    "parser" </> "should_compile" </> "read023.hs",
    "parser" </> "should_fail" </> "ExportCommaComma.hs",
    "parser" </> "should_fail" </> "InfixAppPatErr.hs",
    "parser" </> "should_fail" </> "MultilineStringsInnerTab.hs",
    "parser" </> "should_fail" </> "MultilineStringsSmartQuotes.hs",
    "parser" </> "should_fail" </> "MultilineStringsUnterminated.hs",
    "parser" </> "should_fail" </> "NoBlockArgumentsFail.hs",
    "parser" </> "should_fail" </> "NoBlockArgumentsFail2.hs",
    "parser" </> "should_fail" </> "NoBlockArgumentsFail3.hs",
    "parser" </> "should_fail" </> "NoBlockArgumentsFailArrowCmds.hs",
    "parser" </> "should_fail" </> "NoDoAndIfThenElse.hs",
    "parser" </> "should_fail" </> "NoNumericUnderscores0.hs",
    "parser" </> "should_fail" </> "NoNumericUnderscores1.hs",
    "parser" </> "should_fail" </> "NoPatternSynonyms.hs",
    "parser" </> "should_fail" </> "OpaqueParseFail1.hs",
    "parser" </> "should_fail" </> "OpaqueParseFail2.hs",
    "parser" </> "should_fail" </> "OpaqueParseFail3.hs",
    "parser" </> "should_fail" </> "Or1.hs",
    "parser" </> "should_fail" </> "OrPatInExprErr.hs",
    "parser" </> "should_fail" </> "ParserNoForallUnicode.hs",
    "parser" </> "should_fail" </> "ParserNoLambdaCase.hs",
    "parser" </> "should_fail" </> "ParserNoMultiWayIf.hs",
    "parser" </> "should_fail" </> "ParserNoTH1.hs",
    "parser" </> "should_fail" </> "ParserNoTH2.hs",
    "parser" </> "should_fail" </> "RecordDotSyntaxFail0.hs",
    "parser" </> "should_fail" </> "RecordDotSyntaxFail1.hs",
    "parser" </> "should_fail" </> "RecordDotSyntaxFail2.hs",
    "parser" </> "should_fail" </> "RecordDotSyntaxFail3.hs",
    "parser" </> "should_fail" </> "RecordDotSyntaxFail4.hs",
    "parser" </> "should_fail" </> "RecordDotSyntaxFail6.hs",
    "parser" </> "should_fail" </> "RecordDotSyntaxFail7.hs",
    "parser" </> "should_fail" </> "SuffixAtFail.hs",
    "parser" </> "should_fail" </> "T10196Fail1.hs",
    "parser" </> "should_fail" </> "T10196Fail2.hs",
    "parser" </> "should_fail" </> "T10498a.hs",
    "parser" </> "should_fail" </> "T10498b.hs",
    "parser" </> "should_fail" </> "T12045d.hs",
    "parser" </> "should_fail" </> "T12051.hs",
    "parser" </> "should_fail" </> "T12429.hs",
    "parser" </> "should_fail" </> "T12610.hs",
    "parser" </> "should_fail" </> "T13260.hs",
    "parser" </> "should_fail" </> "T1344a.hs",
    "parser" </> "should_fail" </> "T1344b.hs",
    "parser" </> "should_fail" </> "T1344c.hs",
    "parser" </> "should_fail" </> "T13450.hs",
    "parser" </> "should_fail" </> "T13450TH.hs",
    "parser" </> "should_fail" </> "T14588.hs",
    "parser" </> "should_fail" </> "T15730.hs",
    "parser" </> "should_fail" </> "T15730b.hs",
    "parser" </> "should_fail" </> "T15849.hs",
    "parser" </> "should_fail" </> "T16270.hs",
    "parser" </> "should_fail" </> "T16270h.hs",
    "parser" </> "should_fail" </> "T16999.hs",
    "parser" </> "should_fail" </> "T17162.hs",
    "parser" </> "should_fail" </> "T17865.hs",
    "parser" </> "should_fail" </> "T17879a.hs",
    "parser" </> "should_fail" </> "T17879b.hs",
    "parser" </> "should_fail" </> "T18251a.hs",
    "parser" </> "should_fail" </> "T18251b.hs",
    "parser" </> "should_fail" </> "T18251f.hs",
    "parser" </> "should_fail" </> "T19504.hs",
    "parser" </> "should_fail" </> "T19928.hs",
    "parser" </> "should_fail" </> "T20609.hs",
    "parser" </> "should_fail" </> "T20609a.hs",
    "parser" </> "should_fail" </> "T20609b.hs",
    "parser" </> "should_fail" </> "T20609c.hs",
    "parser" </> "should_fail" </> "T20609d.hs",
    "parser" </> "should_fail" </> "T21843a.hs",
    "parser" </> "should_fail" </> "T21843b.hs",
    "parser" </> "should_fail" </> "T21843c.hs",
    "parser" </> "should_fail" </> "T21843d.hs",
    "parser" </> "should_fail" </> "T21843e.hs",
    "parser" </> "should_fail" </> "T21843f.hs",
    "parser" </> "should_fail" </> "T22070.hs",
    "parser" </> "should_fail" </> "T25258a.hs",
    "parser" </> "should_fail" </> "T25258b.hs",
    "parser" </> "should_fail" </> "T25258c.hs",
    "parser" </> "should_fail" </> "T25530.hs",
    "parser" </> "should_fail" </> "T26418.hs",
    "parser" </> "should_fail" </> "T3095.hs",
    "parser" </> "should_fail" </> "T3153.hs",
    "parser" </> "should_fail" </> "T3751.hs",
    "parser" </> "should_fail" </> "T3811.hs",
    "parser" </> "should_fail" </> "T3811b.hs",
    "parser" </> "should_fail" </> "T3811d.hs",
    "parser" </> "should_fail" </> "T3811e.hs",
    "parser" </> "should_fail" </> "T3811f.hs",
    "parser" </> "should_fail" </> "T5425.hs",
    "parser" </> "should_fail" </> "T8258NoGADTs.hs",
    "parser" </> "should_fail" </> "T8431.hs",
    "parser" </> "should_fail" </> "T8501a.hs",
    "parser" </> "should_fail" </> "T8501b.hs",
    "parser" </> "should_fail" </> "T8506.hs",
    "parser" </> "should_fail" </> "T9225.hs",
    "parser" </> "should_fail" </> "T984.hs",
    "parser" </> "should_fail" </> "cmdFail001.hs",
    "parser" </> "should_fail" </> "cmdFail002.hs",
    "parser" </> "should_fail" </> "cmdFail003.hs",
    "parser" </> "should_fail" </> "cmdFail004.hs",
    "parser" </> "should_fail" </> "cmdFail005.hs",
    "parser" </> "should_fail" </> "cmdFail006.hs",
    "parser" </> "should_fail" </> "cmdFail007.hs",
    "parser" </> "should_fail" </> "cmdFail008.hs",
    "parser" </> "should_fail" </> "cmdFail009.hs",
    "parser" </> "should_fail" </> "cmdFail010.hs",
    "parser" </> "should_fail" </> "cmdFail011.hs",
    "parser" </> "should_fail" </> "patFail001.hs",
    "parser" </> "should_fail" </> "patFail002.hs",
    "parser" </> "should_fail" </> "patFail003.hs",
    "parser" </> "should_fail" </> "patFail004.hs",
    "parser" </> "should_fail" </> "patFail005.hs",
    "parser" </> "should_fail" </> "patFail006.hs",
    "parser" </> "should_fail" </> "patFail007.hs",
    "parser" </> "should_fail" </> "patFail008.hs",
    "parser" </> "should_fail" </> "patFail009.hs",
    "parser" </> "should_fail" </> "position001.hs",
    "parser" </> "should_fail" </> "position002.hs",
    "parser" </> "should_fail" </> "proposal-229c.hs",
    "parser" </> "should_fail" </> "readFail002.hs",
    "parser" </> "should_fail" </> "readFail004.hs",
    "parser" </> "should_fail" </> "readFail005.hs",
    "parser" </> "should_fail" </> "readFail006.hs",
    "parser" </> "should_fail" </> "readFail007.hs",
    "parser" </> "should_fail" </> "readFail009.hs",
    "parser" </> "should_fail" </> "readFail011.hs",
    "parser" </> "should_fail" </> "readFail012.hs",
    "parser" </> "should_fail" </> "readFail013.hs",
    "parser" </> "should_fail" </> "readFail014.hs",
    "parser" </> "should_fail" </> "readFail015.hs",
    "parser" </> "should_fail" </> "readFail017.hs",
    "parser" </> "should_fail" </> "readFail018.hs",
    "parser" </> "should_fail" </> "readFail019.hs",
    "parser" </> "should_fail" </> "readFail020.hs",
    "parser" </> "should_fail" </> "readFail022.hs",
    "parser" </> "should_fail" </> "readFail024.hs",
    "parser" </> "should_fail" </> "readFail025.hs",
    "parser" </> "should_fail" </> "readFail026.hs",
    "parser" </> "should_fail" </> "readFail027.hs",
    "parser" </> "should_fail" </> "readFail031.hs",
    "parser" </> "should_fail" </> "readFail033.hs",
    "parser" </> "should_fail" </> "readFail034.hs",
    "parser" </> "should_fail" </> "readFail040.hs",
    "parser" </> "should_fail" </> "readFail047.hs",
    "parser" </> "should_fail" </> "readFailTraditionalRecords1.hs",
    "parser" </> "should_fail" </> "readFailTraditionalRecords2.hs",
    "parser" </> "should_fail" </> "readFailTraditionalRecords3.hs",
    "parser" </> "should_fail" </> "strictnessDataCon_A.hs",
    "parser" </> "should_fail" </> "strictnessDataCon_B.hs",
    "parser" </> "should_fail" </> "typeopsDataCon_A.hs",
    "parser" </> "should_fail" </> "typeopsDataCon_B.hs",
    "parser" </> "should_fail" </> "typeops_A.hs",
    "parser" </> "should_fail" </> "typeops_B.hs",
    "parser" </> "should_fail" </> "typeops_C.hs",
    "parser" </> "should_fail" </> "typeops_D.hs",
    "parser" </> "should_fail" </> "unpack_before_opr.hs",
    "parser" </> "should_fail" </> "unpack_empty_type.hs",
    "parser" </> "unicode" </> "T10907.hs",
    "parser" </> "unicode" </> "T1744.hs",
    "parser" </> "unicode" </> "T18158b.hs",
    "parser" </> "unicode" </> "T18225B.hs",
    "parser" </> "unicode" </> "utf8_001.hs",
    "parser" </> "unicode" </> "utf8_002.hs",
    "parser" </> "unicode" </> "utf8_003.hs",
    "parser" </> "unicode" </> "utf8_004.hs",
    "parser" </> "unicode" </> "utf8_005.hs",
    "parser" </> "unicode" </> "utf8_010.hs",
    "parser" </> "unicode" </> "utf8_011.hs",
    "parser" </> "unicode" </> "utf8_020.hs",
    "parser" </> "unicode" </> "utf8_021.hs",
    "parser" </> "unicode" </> "utf8_022.hs",
    "parser" </> "unicode" </> "utf8_023.hs",
    "partial-sigs" </> "should_compile" </> "T14217.hs",
    "patsyn" </> "should_fail" </> "T10426.hs",
    "patsyn" </> "should_fail" </> "export-syntax.hs",
    "patsyn" </> "should_fail" </> "import-syntax.hs",
    "perf" </> "compiler" </> "T12234.hs",
    "plugins" </> "T20803a.hs",
    "profiling" </> "should_compile" </> "T19894" </> "inline.hs",
    "profiling" </> "should_fail" </> "T17916.hs",
    "profiling" </> "should_fail" </> "proffail001.hs",
    "programs" </> "barton-mangler-bug" </> "Bug.hs",
    "programs" </> "joao-circular" </> "Funcs_Lexer.hs",
    "programs" </> "joao-circular" </> "LrcPrelude.hs",
    "qualifieddo" </> "should_fail" </> "qdofail002.hs",
    "qualifieddo" </> "should_fail" </> "qdofail005.hs",
    "quasiquotation" </> "T5204.hs",
    "quotes" </> "T20893.hs",
    "quotes" </> "T3572.hs",
    "quotes" </> "T4056.hs",
    "quotes" </> "T4169.hs",
    "quotes" </> "T4170.hs",
    "quotes" </> "T8455.hs",
    "quotes" </> "T8759a.hs",
    "quotes" </> "T9824.hs",
    "quotes" </> "TH_abstractFamily.hs",
    "quotes" </> "TH_bracket1.hs",
    "quotes" </> "TH_bracket2.hs",
    "quotes" </> "TH_bracket3.hs",
    "quotes" </> "TH_ppr1.hs",
    "quotes" </> "TH_scope.hs",
    "quotes" </> "TH_spliceViewPat" </> "A.hs",
    "rename" </> "should_fail" </> "T12879.hs",
    "rename" </> "should_fail" </> "T14907a.hs",
    "rename" </> "should_fail" </> "T9437.hs",
    "rename" </> "should_fail" </> "rnfail016.hs",
    "rename" </> "should_fail" </> "rnfail016a.hs",
    "rename" </> "should_fail" </> "rnfail052.hs",
    "roles" </> "should_fail" </> "Roles7.hs",
    "saks" </> "should_fail" </> "saks_fail007.hs",
    "saks" </> "should_fail" </> "saks_fail024.hs",
    "saks" </> "should_fail" </> "saks_fail025.hs",
    "simplCore" </> "should_compile" </> "T13658.hs",
    "simplCore" </> "should_compile" </> "T5821.hs",
    "simplCore" </> "should_fail" </> "T25117b.hs",
    "splice-imports" </> "SI27.hs",
    "stage1" </> "T2632.hs",
    "th" </> "T10279.hs",
    "th" </> "T10638.hs",
    "th" </> "T10819.hs",
    "th" </> "T10891.hs",
    "th" </> "T11484.hs",
    "th" </> "T16326_TH.hs",
    "th" </> "T16980a.hs",
    "th" </> "T3177.hs",
    "th" </> "T3177a.hs",
    "th" </> "T5217.hs",
    "th" </> "T6018th.hs",
    "th" </> "T8807.hs",
    "th" </> "T9209.hs",
    "th" </> "TH_ExplicitForAllRules_a.hs",
    "th" </> "TH_class1.hs",
    "th" </> "TH_dataD1.hs",
    "th" </> "TH_implicitParams.hs",
    "th" </> "TH_lookupName.hs",
    "th" </> "TH_raiseErr1.hs",
    "th" </> "TH_recover.hs",
    "th" </> "TH_recursiveDo.hs",
    "th" </> "TH_recursiveDoImport.hs",
    "th" </> "TH_reifyDecl1.hs",
    "th" </> "TH_reifyDecl2.hs",
    "th" </> "TH_reifyExplicitForAllFams.hs",
    "th" </> "TH_reifyInstances.hs",
    "th" </> "TH_reifyLinear.hs",
    "th" </> "TH_reifyMkName.hs",
    "th" </> "TH_repE2.hs",
    "th" </> "TH_repGuard.hs",
    "th" </> "TH_repGuardOutput.hs",
    "th" </> "TH_repPatSig.hs",
    "th" </> "TH_repPatSigTVar.hs",
    "th" </> "TH_repPrim.hs",
    "th" </> "TH_repPrim2.hs",
    "th" </> "TH_repPrimOutput.hs",
    "th" </> "TH_repPrimOutput2.hs",
    "th" </> "TH_sections.hs",
    "th" </> "TH_spliceD2.hs",
    "th" </> "TH_spliceDecl1.hs",
    "th" </> "TH_spliceDecl2.hs",
    "th" </> "TH_spliceDecl3.hs",
    "th" </> "TH_spliceE1.hs",
    "th" </> "TH_spliceE3.hs",
    "th" </> "TH_spliceE4.hs",
    "th" </> "TH_spliceExpr1.hs",
    "th" </> "TH_tf1.hs",
    "th" </> "TH_tf3.hs",
    "th" </> "TH_unresolvedInfix.hs",
    "th" </> "TH_unresolvedInfix2.hs",
    "typecheck" </> "should_compile" </> "tc134.hs",
    "typecheck" </> "should_fail" </> "ExplicitSpecificity3.hs",
    "typecheck" </> "should_fail" </> "ExplicitSpecificity8.hs",
    "typecheck" </> "should_fail" </> "T13446.hs",
    "typecheck" </> "should_fail" </> "T14761b.hs",
    "typecheck" </> "should_fail" </> "T2126.hs",
    "typecheck" </> "should_fail" </> "T3155.hs",
    "typecheck" </> "should_fail" </> "T9634.hs",
    "typecheck" </> "should_fail" </> "tcfail089.hs",
    "typecheck" </> "should_fail" </> "tcfail166.hs",
    "typecheck" </> "should_fail" </> "tcfail183.hs",
    "typecheck" </> "should_run" </> "T4809.hs",
    "unboxedsums" </> "UnboxedSumsTH_Fail.hs",
    "unboxedsums" </> "unboxedsums4.hs",
    "vdq-rta" </> "should_fail" </> "T24159_pat_parse_error_1.hs",
    "vdq-rta" </> "should_fail" </> "T24159_pat_parse_error_2.hs",
    "vdq-rta" </> "should_fail" </> "T24159_pat_parse_error_3.hs",
    "vdq-rta" </> "should_fail" </> "T24159_pat_parse_error_4.hs",
    "vdq-rta" </> "should_fail" </> "T24159_pat_parse_error_5.hs",
    "vdq-rta" </> "should_fail" </> "T24159_pat_parse_error_6.hs",
    "warnings" </> "should_fail" </> "CaretDiagnostics2.hs",
    "warnings" </> "should_fail" </> "SpecEMultipleTys.hs"
  ]

----------------------------------------------------------------------------
-- Obtaining one

-- | One thing to format, and what it should come out as if that is known.
data Example = Example
  { -- | Where the corpus puts it, relative to the corpus root, which is
    -- what names the test: the absolute path runs through a cache directory
    -- that differs on every machine.
    exampleName :: FilePath,
    -- | The file to format, as an absolute path on this machine.
    exampleInput :: FilePath,
    -- | The file holding what the corpus says formatting should produce, if
    -- it says. 'Nothing' for a corpus that ships no expected outputs, and
    -- for an example within one that happens to have none.
    exampleReference :: Maybe FilePath,
    -- | The file holding what formatting should produce with
    -- 'alternativeSettings', if the corpus says: one of their own where
    -- they give something else, the one above where they do not.
    exampleAlternativeReference :: Maybe FilePath,
    -- | What the package around it puts in force, already resolved from its
    -- @.cabal@ file. 'Nothing' for a corpus whose examples are not modules
    -- of a package, see 'corpusInPackages', and for one whose package could
    -- not be read.
    exampleExtensions :: Maybe Defaults,
    -- | The modules the project around it declares, each with the names of
    -- the packages that declare it, and none for a corpus whose examples
    -- are not modules of a package.
    exampleOwnModules :: Map Text (Set Text)
  }
  deriving (Eq, Show)

-- | Get a corpus, fetching and unpacking it if this machine does not have
-- it yet.
--
-- Fetching happens once: an unpacked corpus is left in place and found
-- again, and a download interrupted half way leaves nothing behind to be
-- mistaken for a complete one. 'Left' says why a corpus that is not here
-- could not be fetched; a vendored corpus is always here.
--
-- The archive of a 'Fetched' corpus is deleted once unpacked; Hackage
-- releases keep theirs, which the fixity benchmarks read.
obtain :: Corpus -> IO (Either Text [Example])
obtain corpus = case corpusSource corpus of
  Vendored dir -> Right <$> examplesIn corpus dir [dir]
  Fetched url root -> do
    home <- corpusCache
    let unpacked = home </> corpusName corpus
        archive = unpacked <> ".tar.gz"
    createDirectoryIfMissing True home
    fetch url archive unpacked >>= \case
      Left problem -> pure (Left problem)
      Right () -> do
        removePathForcibly archive
        Right <$> examplesIn corpus (unpacked </> root) [unpacked </> root]
  HackageReleases releases -> do
    home <- corpusCache
    let root = home </> corpusName corpus
    createDirectoryIfMissing True root
    inTurn root releases >>= \case
      Left problem -> pure (Left problem)
      Right () -> Right <$> examplesIn corpus root (fmap (root </>) releases)
  where
    inTurn _ [] = pure (Right ())
    inTurn root (name : rest) =
      fetch (hackage name) (root </> name <> ".tar.gz") (root </> name) >>= \case
        Left problem -> pure (Left problem)
        Right () -> inTurn root rest

-- | Put an archive's contents where they are wanted, if they are not there.
--
-- Fetching happens once and unpacking happens once, and either step already
-- done is skipped.
fetch ::
  (Url 'Https, Option 'Https) -> FilePath -> FilePath -> IO (Either Text ())
fetch url archive unpacked =
  doesDirectoryExist unpacked >>= \case
    True -> pure (Right ())
    False -> do
      have <- doesFileExist archive
      got <- if have then pure (Right ()) else download url archive
      either (pure . Left) (const (unpackTo archive unpacked)) got

-- | Where Hackage keeps a release's sources.
hackage :: String -> (Url 'Https, Option 'Https)
hackage name =
  ( https "hackage.haskell.org"
      /: "package"
      /: T.pack name
      /: T.pack (name <> ".tar.gz"),
    mempty
  )

-- | Where corpora are kept.
--
-- Beside the fixity cache, and for the same reason: it is data about the
-- outside world that is expensive to obtain and cheap to keep.
corpusCache :: IO FilePath
corpusCache =
  lookupEnv "TILIA_CORPUS_DIR" >>= \case
    Just dir -> pure dir
    Nothing -> (</> "corpus") <$> getXdgDirectory XdgCache "tilia"

----------------------------------------------------------------------------
-- Fetching

-- | Fetch an archive.
--
-- Written to a temporary name and moved into place. Anything that leaves a
-- partial file under the real name would be taken for a complete download
-- on the next run and never fetched again.
--
-- 'Left' says why the fetch failed. Only the fetch is caught: a file that
-- cannot be written is a fault worth hearing about.
download :: (Url 'Https, Option 'Https) -> FilePath -> IO (Either Text ())
download (url, query) dest =
  try get >>= \case
    Left (e :: HttpException) -> pure (Left (explain e))
    Right bytes
      | not (gzipped bytes) -> pure (Left "the answer was not an archive")
      | otherwise -> do
          BL.writeFile partial bytes
          Right <$> renameFile partial dest
  where
    partial = dest <> ".part"
    -- The two bytes every gzip stream opens with.
    gzipped = (== [0x1f, 0x8b]) . BL.unpack . BL.take 2
    get =
      runReq defaultHttpConfig $
        responseBody <$> req GET url NoReqBody lbsResponse query
    explain = \case
      VanillaHttpException (HTTP.HttpExceptionRequest _ reason) ->
        flatten reason
      other -> flatten other
    flatten :: Show a => a -> Text
    flatten = T.take 200 . T.unwords . T.words . T.pack . show

-- | Unpack the Haskell files of an archive, dropping its top-level
-- directory.
unpackTo :: FilePath -> FilePath -> IO (Either Text ())
unpackTo archive dest = do
  removePathForcibly staging
  outcome <- quietly (Left "could not unpack") $ do
    bytes <- BL.readFile archive
    Tar.foldEntries
      write
      (pure ())
      (const (pure ()))
      (Tar.read (GZip.decompress bytes))
    pure (Right ())
  case outcome of
    Left problem -> do
      removePathForcibly staging
      pure (Left (problem <> " " <> T.pack archive))
    Right () -> do
      there <- doesDirectoryExist staging
      if there
        then Right <$> renameDirectory staging dest
        else pure (Left ("nothing to unpack in " <> T.pack archive))
  where
    staging = dest <> ".part"

    write entry rest = do
      case Tar.entryContent entry of
        Tar.NormalFile content _
          | Just path <- beneathTop (Tar.entryPath entry),
            any (`isSuffixOf` path) (".cabal" : sourceExtensions) -> do
              createDirectoryIfMissing True (takeDirectory (staging </> path))
              BL.writeFile (staging </> path) content
        _ -> pure ()
      rest
    beneathTop path = case splitDirectories path of
      (_ : rest@(_ : _)) | all safe rest -> Just (foldr1 (</>) rest)
      _ -> Nothing
    safe part = part /= ".." && not ("/" `isPrefixOf` part)

----------------------------------------------------------------------------
-- Enumerating

-- | Every example in these directories of an unpacked corpus, named from
-- its root, in a settled order.
examplesIn ::
  -- | The corpus.
  Corpus ->
  -- | Where it is unpacked, which the examples are named from.
  FilePath ->
  -- | The directories to look in.
  [FilePath] ->
  IO [Example]
examplesIn corpus root dirs = do
  found <- sort . concat <$> traverse haskellFilesIn dirs
  reader <- packageReaderFor corpus
  own <- ownModulesReaderFor corpus
  let present = Set.fromList found
      example f reference alternative =
        Example (nameOf f) f reference alternative <$> reader f <*> own f
  case corpusReference corpus of
    NoReference -> traverse (\f -> example f Nothing Nothing) found
    ReferenceMarked mark ->
      forM (filter (not . answerTo mark present) found) $ \f -> do
        reference <- existing (answerFile mark f)
        primed <- existing (answerFile (mark <> "'") f)
        example f reference (primed <|> reference)
  where
    nameOf f = fromMaybe f (stripPrefix (root <> "/") f)
    existing f = (\there -> f <$ guard there) <$> doesFileExist f
    answerTo mark present f =
      any
        (\stem -> Set.member (stem <> extensionOf f) present)
        (mapMaybe (`withoutSuffix` stemOf f) (marks mark))
    marks mark = [mark <> "'", mark]

-- | Where a corpus with answers keeps one for an example: the example's
-- file with the mark put before its extension, as in @name-out.hs@.
answerFile ::
  -- | The mark.
  String ->
  -- | The example's file.
  FilePath ->
  FilePath
answerFile mark f = stemOf f <> mark <> extensionOf f

-- | A file's name less the extension of a Haskell source file it has.
stemOf :: FilePath -> FilePath
stemOf f =
  fromMaybe f (listToMaybe (mapMaybe (`withoutSuffix` f) sourceExtensions))

-- | The extension of a Haskell source file a file has, if any.
extensionOf :: FilePath -> String
extensionOf f =
  fromMaybe "" (listToMaybe (filter (`isSuffixOf` f) sourceExtensions))

-- | A name less a suffix, if it ends with that suffix.
withoutSuffix ::
  -- | The suffix.
  String ->
  -- | The name.
  String ->
  Maybe String
withoutSuffix suffix name
  | suffix `isSuffixOf` name = Just (take (length name - length suffix) name)
  | otherwise = Nothing

-- | The modules the project around each of a corpus's examples declares,
-- read once for every project.
ownModulesReaderFor ::
  -- | The corpus.
  Corpus ->
  IO (FilePath -> IO (Map Text (Set Text)))
ownModulesReaderFor corpus
  | not (corpusInPackages corpus) = pure (const (pure Map.empty))
  | otherwise = do
      known <- newIORef Map.empty
      pure $ \f ->
        findProjectRoot f >>= \case
          Nothing -> pure Map.empty
          Just root ->
            Map.lookup (prPath root) <$> readIORef known >>= \case
              Just modules -> pure modules
              Nothing -> do
                modules <-
                  either (fail . T.unpack . describeTargetProblem) pure
                    =<< either (pure . Left) projectModules
                    =<< packageFilesOf root
                atomicModifyIORef' known $
                  (,()) . Map.insert (prPath root) modules
                pure modules

-- | What each of a corpus's examples has in force before its own pragmas.
packageReaderFor :: Corpus -> IO (FilePath -> IO (Maybe Defaults))
packageReaderFor corpus
  | not (corpusInPackages corpus) = pure (const (pure Nothing))
  | otherwise = do
      reader <- newPackageReader Nothing Map.empty []
      pure (fmap (either (const Nothing) Just) . reader)
