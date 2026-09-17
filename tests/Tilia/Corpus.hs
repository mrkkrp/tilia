{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Corpora of Haskell to run the formatter over.
module Tilia.Corpus
  ( -- * Corpora
    Corpus (..),
    Source (..),
    Reference (..),
    Expectations (..),
    Lists (..),
    vendoredExamples,
    ormoluExamples,
    ghcTestSuite,
    hackagePackages,

    -- * Obtaining one
    Example (..),
    obtain,
  )
where

import Codec.Archive.Tar qualified as Tar
import Codec.Compression.GZip qualified as GZip
import Control.Exception (SomeException, try)
import Control.Monad (forM)
import Data.ByteString.Lazy qualified as BL
import Data.List (isPrefixOf, isSuffixOf, sort, stripPrefix)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.LanguageExtensions.Type (Extension)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Req
import System.Directory
  ( XdgDirectory (..),
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    getXdgDirectory,
    listDirectory,
    removePathForcibly,
    renameDirectory,
    renameFile,
  )
import System.Environment (lookupEnv)
import System.FilePath (splitDirectories, takeDirectory, (</>))
import Tilia.Cabal.Package (newPackageReader)

----------------------------------------------------------------------------
-- Corpora

-- | Whether a corpus says what the formatted result should look like.
data Reference
  = -- | It does not, so only the properties that hold of any input can be
    -- checked.
    NoReference
  | -- | It does, in a file whose name is the input's with the given mark
    -- put before the extension.
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
  { -- | Examples to leave alone, named relative to the root of the corpus.
    -- A name with no extension stands for a directory and takes everything
    -- under it.
    expectSkip :: [FilePath],
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
    -- package's @default-extensions@ and does not parse without them.
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
            { expectSkip = [],
              expectDeclined =
                [ "other" </> "position-pragmas.hs",
                  "other" </> "cpp" </> "unbalanced.hs",
                  "other" </> "cpp" </> "define-in-a-quasiquote.hs"
                ]
            },
      corpusInPackages = False
    }

-- | Ormolu's examples.
ormoluExamples :: Corpus
ormoluExamples =
  Corpus
    { corpusName = "ormolu-0.9.0.0",
      corpusSource =
        Fetched
          ( https "hackage.haskell.org"
              /: "package"
              /: "ormolu-0.9.0.0"
              /: "ormolu-0.9.0.0.tar.gz",
            mempty
          )
          ("data" </> "examples"),
      corpusReference = ReferenceMarked "-out",
      corpusExpectations =
        Listed Lists {expectSkip = ormoluSkip, expectDeclined = []},
      corpusInPackages = False
    }

-- | GHC's test suite.
ghcTestSuite :: Corpus
ghcTestSuite =
  Corpus
    { corpusName = "ghc-9.10.1-testsuite",
      corpusSource =
        Fetched
          ( https "codeload.github.com"
              /: "ghc"
              /: "ghc"
              /: "tar.gz"
              /: "refs"
              /: "tags"
              /: "ghc-9.10.1-release",
            mempty
          )
          ("testsuite" </> "tests"),
      corpusReference = NoReference,
      corpusExpectations =
        Listed
          Lists
            { expectSkip =
                ["perf" </> "compiler" </> "parsing001.hs"] <> ghcUnreadable,
              expectDeclined = ghcDeclined
            },
      corpusInPackages = False
    }

-- | GHC test suite files the formatter is right to refuse.
ghcDeclined :: [FilePath]
ghcDeclined =
  [ "ghci.debugger" </> "HappyTest.hs",
    "parser" </> "should_compile" </> "ColumnPragma.hs",
    "parser" </> "should_compile" </> "T7118.hs",
    "perf" </> "compiler" </> "T20261.hs",
    "perf" </> "compiler" </> "T5631.hs",
    "programs" </> "joao-circular" </> "Funcs_Parser_Lazy.hs",
    "quasiquotation" </> "T4150.hs"
  ]

-- | Packages from Hackage.
hackagePackages :: Corpus
hackagePackages =
  Corpus
    { corpusName = "hackage",
      corpusSource = HackageReleases hackageReleases,
      corpusReference = NoReference,
      corpusExpectations = Recorded ("corpora" </> "hackage" </> "hackage.manifest"),
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
    "diagrams-core-1.5.1.2",
    "distributed-process-0.7.8",
    "dlist-1.0",
    "esqueleto-3.6.0.3",
    "exceptions-0.10.12",
    "fay-0.24.2.0",
    "free-5.2",
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

-- | Ormolu examples we do not format the way Ormolu does.
ormoluSkip :: [FilePath]
ormoluSkip =
  [ "other" </> "disabling",
    "declaration" </> "value" </> "function" </> "required-type-arguments-2.hs",
    "declaration" </> "data" </> "comment-in-empty-record.hs",
    "import" </> "comment-inside-empty-import-list.hs",
    "other" </> "comment-two-blocks.hs",
    "other" </> "comment-glued-together.hs",
    "other" </> "multiple-blank-line-comment.hs",
    "declaration" </> "type" </> "parens-comments.hs",
    "declaration" </> "value" </> "function" </> "parens-comments.hs",
    "import" </> "comments-inside-imports.hs",
    "import" </> "comment-between-merged-imports.hs",
    "declaration" </> "data" </> "with-comment.hs",
    "declaration" </> "data" </> "record-empty-haddock.hs",
    "other" </> "empty-haddock.hs",
    "declaration" </> "value" </> "function" </> "arrow" </> "proc-do-complex.hs",
    "declaration" </> "value" </> "function" </> "comprehension" </> "transform-multi-line2.hs",
    "declaration" </> "value" </> "function" </> "if-with-comment-next-to-keyword.hs",
    "declaration" </> "value" </> "function" </> "operator-comments-2.hs",
    "declaration" </> "value" </> "function" </> "record" </> "wildcard-comments-0.hs",
    "declaration" </> "value" </> "function" </> "record" </> "wildcard-comments-1.hs",
    "other" </> "pragma-comments-after.hs",
    "declaration" </> "value" </> "function" </> "infix" </> "esqueleto-0.hs",
    "declaration" </> "value" </> "function" </> "infix" </> "esqueleto-1.hs",
    "declaration" </> "class" </> "default-signatures.hs",
    "declaration" </> "type-families" </> "closed-type-family" </> "with-comments.hs",
    "declaration" </> "deriving" </> "singleline.hs",
    "declaration" </> "deriving" </> "multiline.hs",
    "declaration" </> "deriving" </> "overlapping.hs",
    "declaration" </> "warning" </> "warning-single-line.hs"
  ]
    <> ormoluUnreadable

-- | Ormolu examples GHC's own parser cannot read.
ormoluUnreadable :: [FilePath]
ormoluUnreadable =
  [ "declaration" </> "class" </> "type-operators3.hs",
    "declaration" </> "data" </> "datatype-contexts.hs",
    "declaration" </> "foreign" </> "foreign-import-multiline.hs",
    "declaration" </> "value" </> "function" </> "application-1.hs",
    "declaration" </> "value" </> "function" </> "application-2.hs",
    "declaration" </> "value" </> "function" </> "arrow" </> "proc-cases.hs",
    "declaration" </> "value" </> "function" </> "arrow" </> "proc-do-simple1.hs",
    "declaration" </> "value" </> "function" </> "block-arguments.hs",
    "declaration" </> "value" </> "function" </> "case-empty.hs",
    "declaration" </> "value" </> "function" </> "do-single-line-lambda-case.hs",
    "declaration" </> "value" </> "function" </> "if-multi-line.hs",
    "declaration" </> "value" </> "function" </> "infix" </> "hanging.hs",
    "declaration" </> "value" </> "function" </> "let-multi-line.hs",
    "declaration" </> "value" </> "function" </> "let-single-line.hs",
    "declaration" </> "value" </> "function" </> "negation.hs",
    "declaration" </> "value" </> "function" </> "negative-literals.hs",
    "declaration" </> "value" </> "function" </> "pattern" </> "or-patterns.hs",
    "declaration" </> "value" </> "function" </> "type-applications.hs",
    "other" </> "comment-before-hanging.hs",
    "other" </> "cpp" </> "continuation.hs",
    "other" </> "cpp" </> "cpp-and-imports.hs",
    "other" </> "cpp" </> "lonely-hash.hs",
    "other" </> "cpp" </> "separation-0a.hs",
    "other" </> "cpp" </> "separation-0b.hs",
    "other" </> "cpp" </> "separation-1a.hs",
    "other" </> "cpp" </> "separation-1b.hs",
    "other" </> "cpp" </> "separation-2a.hs",
    "other" </> "cpp" </> "separation-2b.hs",
    "other" </> "cpp" </> "shifted.hs",
    "other" </> "cpp" </> "simple-import.hs",
    "other" </> "necessary-brackets.hs"
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
    "cabal" </> "fileStatus.hs",
    "codeGen" </> "should_run" </> "CheckBoundsOK.hs",
    "codeGen" </> "should_run" </> "T10245.hs",
    "codeGen" </> "should_run" </> "T12855.hs",
    "codeGen" </> "should_run" </> "T2080.hs",
    "codeGen" </> "should_run" </> "T7600.hs",
    "codeGen" </> "should_run" </> "cas_int.hs",
    "codeGen" </> "should_run" </> "cgrun044.hs",
    "codeGen" </> "should_run" </> "cgrun071.hs",
    "codeGen" </> "should_run" </> "cgrun072.hs",
    "codeGen" </> "should_run" </> "cgrun075.hs",
    "codeGen" </> "should_run" </> "cgrun076.hs",
    "codeGen" </> "should_run" </> "cgrun077.hs",
    "codeGen" </> "should_run" </> "cgrun079.hs",
    "codeGen" </> "should_run" </> "cgrun080.hs",
    "concurrent" </> "should_run" </> "T5611.hs",
    "concurrent" </> "should_run" </> "T5611a.hs",
    "concurrent" </> "should_run" </> "conc036.hs",
    "concurrent" </> "should_run" </> "conc037.hs",
    "concurrent" </> "should_run" </> "conc038.hs",
    "concurrent" </> "should_run" </> "foreignInterruptible.hs",
    "corelint" </> "T21115.hs",
    "deSugar" </> "should_run" </> "T5742.hs",
    "dependent" </> "should_fail" </> "RenamingStar.hs",
    "dmdanal" </> "should_compile" </> "T9208.hs",
    "driver" </> "FullGHCVersion.hs",
    "driver" </> "T10869.hs",
    "driver" </> "T10869A.hs",
    "driver" </> "T10970.hs",
    "driver" </> "T11763.hs",
    "driver" </> "T12135.hs",
    "driver" </> "T12674" </> "-T12674.hs",
    "driver" </> "T12752pass.hs",
    "driver" </> "T16167.hs",
    "driver" </> "T16476a.hs",
    "driver" </> "T16476b.hs",
    "driver" </> "T16521" </> "A.hs",
    "driver" </> "T17786.hs",
    "driver" </> "T2464.hs",
    "driver" </> "T3389.hs",
    "driver" </> "T8526" </> "A.hs",
    "driver" </> "bug1677" </> "Foo.hs",
    "driver" </> "multipleHomeUnits" </> "c-file" </> "C.hs",
    "driver" </> "multipleHomeUnits" </> "cpp-includes" </> "CPPIncludes.hs",
    "driver" </> "multipleHomeUnits" </> "cpp-includes" </> "CPPIncludes_Down.hs",
    "driver" </> "recomp011" </> "Main.hs",
    "driver" </> "recomp021" </> "A.hs",
    "driver" </> "should_fail" </> "T12752.hs",
    "eyeball" </> "inline2.hs",
    "ffi" </> "should_fail" </> "capi_wrapper.hs",
    "ffi" </> "should_fail" </> "ccall_value.hs",
    "ffi" </> "should_run" </> "T22159.hs",
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
    "ghci" </> "should_run" </> "PackedDataCon" </> "ByteCode.hs",
    "ghci" </> "should_run" </> "PackedDataCon" </> "Obj.hs",
    "ghci" </> "should_run" </> "UnboxedTuples" </> "ByteCode.hs",
    "ghci" </> "should_run" </> "UnboxedTuples" </> "Obj.hs",
    "ghci" </> "should_run" </> "UnliftedDataTypeInterp" </> "ByteCode.hs",
    "ghci" </> "should_run" </> "UnliftedDataTypeInterp" </> "Obj.hs",
    "haddock" </> "should_compile_flag_haddock" </> "haddockA004.hs",
    "haddock" </> "should_compile_flag_haddock" </> "haddockA011.hs",
    "haddock" </> "should_compile_flag_haddock" </> "haddockA041.hs",
    "haddock" </> "should_compile_noflag_haddock" </> "haddockC004.hs",
    "haddock" </> "should_compile_noflag_haddock" </> "haddockC011.hs",
    "haddock" </> "should_fail_flag_haddock" </> "haddockE003.hs",
    "hiefile" </> "should_compile" </> "CPP.hs",
    "hiefile" </> "should_compile" </> "T22416.hs",
    "hiefile" </> "should_compile" </> "hie002.hs",
    "indexed-types" </> "should_compile" </> "T12538.hs",
    "javascript" </> "T23346.hs",
    "lib" </> "integer" </> "IntegerConversionRules.hs",
    "linear" </> "should_fail" </> "LinearNoExt.hs",
    "linear" </> "should_fail" </> "LinearNoExtU.hs",
    "linear" </> "should_fail" </> "T20083.hs",
    "mdo" </> "should_compile" </> "mdo001.hs",
    "mdo" </> "should_compile" </> "mdo002.hs",
    "mdo" </> "should_compile" </> "mdo003.hs",
    "mdo" </> "should_compile" </> "mdo004.hs",
    "mdo" </> "should_compile" </> "mdo005.hs",
    "mdo" </> "should_compile" </> "mdo006.hs",
    "mdo" </> "should_fail" </> "mdofail001.hs",
    "mdo" </> "should_fail" </> "mdofail002.hs",
    "mdo" </> "should_fail" </> "mdofail003.hs",
    "mdo" </> "should_fail" </> "mdofail004.hs",
    "mdo" </> "should_fail" </> "mdofail005.hs",
    "mdo" </> "should_fail" </> "mdofail006.hs",
    "mdo" </> "should_run" </> "mdorun001.hs",
    "mdo" </> "should_run" </> "mdorun002.hs",
    "mdo" </> "should_run" </> "mdorun003.hs",
    "mdo" </> "should_run" </> "mdorun005.hs",
    "module" </> "Mod178_2.hs",
    "module" </> "T11432.hs",
    "module" </> "T11432a.hs",
    "module" </> "T12026.hs",
    "module" </> "mod183.hs",
    "module" </> "mod69.hs",
    "module" </> "mod70.hs",
    "module" </> "mod76.hs",
    "module" </> "mod89.hs",
    "module" </> "mod98.hs",
    "numeric" </> "should_run" </> "T12136.hs",
    "numeric" </> "should_run" </> "T20291.hs",
    "numeric" </> "should_run" </> "foundation.hs",
    "parser" </> "should_compile" </> "T10582.hs",
    "parser" </> "should_compile" </> "T15279.hs",
    "parser" </> "should_compile" </> "read023.hs",
    "parser" </> "should_compile" </> "read039.hs",
    "parser" </> "should_compile" </> "read046.hs",
    "parser" </> "should_compile" </> "read058.hs",
    "parser" </> "should_fail" </> "ExportCommaComma.hs",
    "parser" </> "should_fail" </> "InfixAppPatErr.hs",
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
    "parser" </> "should_fail" </> "T15730.hs",
    "parser" </> "should_fail" </> "T15730b.hs",
    "parser" </> "should_fail" </> "T15849.hs",
    "parser" </> "should_fail" </> "T16270.hs",
    "parser" </> "should_fail" </> "T16270h.hs",
    "parser" </> "should_fail" </> "T16999.hs",
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
    "parser" </> "should_fail" </> "T3095.hs",
    "parser" </> "should_fail" </> "T3153.hs",
    "parser" </> "should_fail" </> "T3751.hs",
    "parser" </> "should_fail" </> "T3811.hs",
    "parser" </> "should_fail" </> "T3811b.hs",
    "parser" </> "should_fail" </> "T3811d.hs",
    "parser" </> "should_fail" </> "T3811e.hs",
    "parser" </> "should_fail" </> "T3811f.hs",
    "parser" </> "should_fail" </> "T5425.hs",
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
    "perf" </> "compiler" </> "T14683.hs",
    "perf" </> "compiler" </> "T18698" </> "T18698.hs",
    "perf" </> "should_run" </> "T13623.hs",
    "plugins" </> "T20803a.hs",
    "plugins" </> "plugin-recomp" </> "Common.hs",
    "primops" </> "should_run" </> "T4442.hs",
    "primops" </> "should_run" </> "UnalignedAddrPrimOps.hs",
    "printer" </> "Ppr010.hs",
    "printer" </> "Ppr027.hs",
    "profiling" </> "should_compile" </> "T19894" </> "Fold.hs",
    "profiling" </> "should_compile" </> "T19894" </> "Operations.hs",
    "profiling" </> "should_compile" </> "T19894" </> "Step.hs",
    "profiling" </> "should_compile" </> "T19894" </> "StreamD.hs",
    "profiling" </> "should_compile" </> "T19894" </> "StreamK.hs",
    "profiling" </> "should_compile" </> "T19894" </> "Unfold.hs",
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
    "rename" </> "should_fail" </> "T9032.hs",
    "rename" </> "should_fail" </> "T9437.hs",
    "rename" </> "should_fail" </> "rnfail016.hs",
    "rename" </> "should_fail" </> "rnfail016a.hs",
    "roles" </> "should_fail" </> "Roles7.hs",
    "rts" </> "T12497.hs",
    "rts" </> "linker" </> "T20494.hs",
    "rts" </> "linker" </> "T5435.hs",
    "rts" </> "stack002.hs",
    "runghc" </> "T6132.hs",
    "safeHaskell" </> "flags" </> "Flags01.hs",
    "safeHaskell" </> "safeLanguage" </> "SafeLang18.hs",
    "saks" </> "should_fail" </> "saks_fail007.hs",
    "saks" </> "should_fail" </> "saks_fail024.hs",
    "saks" </> "should_fail" </> "saks_fail025.hs",
    "simplCore" </> "T9646" </> "Main.hs",
    "simplCore" </> "T9646" </> "StrictPrim.hs",
    "simplCore" </> "T9646" </> "Type.hs",
    "simplCore" </> "should_compile" </> "T13658.hs",
    "simplCore" </> "should_compile" </> "T21694.hs",
    "simplCore" </> "should_compile" </> "T8832.hs",
    "simplCore" </> "should_run" </> "T21575.hs",
    "stage1" </> "T2632.hs",
    "th" </> "T10279.hs",
    "th" </> "T10638.hs",
    "th" </> "T10819.hs",
    "th" </> "T10891.hs",
    "th" </> "T11484.hs",
    "th" </> "T16180.hs",
    "th" </> "T16326_TH.hs",
    "th" </> "T16980a.hs",
    "th" </> "T23309A.hs",
    "th" </> "T23378A.hs",
    "th" </> "T2817.hs",
    "th" </> "T3177.hs",
    "th" </> "T3177a.hs",
    "th" </> "T4436.hs",
    "th" </> "T5217.hs",
    "th" </> "T6018th.hs",
    "th" </> "T8807.hs",
    "th" </> "T9209.hs",
    "th" </> "TH_ExplicitForAllRules_a.hs",
    "th" </> "TH_class1.hs",
    "th" </> "TH_dataD1.hs",
    "th" </> "TH_foreignCallingConventions.hs",
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
    "th" </> "TH_reifyLocalDefs.hs",
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
    "th" </> "TH_spliceGuard.hs",
    "th" </> "TH_tf1.hs",
    "th" </> "TH_tf3.hs",
    "th" </> "TH_unresolvedInfix.hs",
    "th" </> "TH_unresolvedInfix2.hs",
    "typecheck" </> "should_compile" </> "FloatFDs.hs",
    "typecheck" </> "should_compile" </> "tc134.hs",
    "typecheck" </> "should_fail" </> "ExplicitSpecificity3.hs",
    "typecheck" </> "should_fail" </> "ExplicitSpecificity8.hs",
    "typecheck" </> "should_fail" </> "T13446.hs",
    "typecheck" </> "should_fail" </> "T14761b.hs",
    "typecheck" </> "should_fail" </> "T2126.hs",
    "typecheck" </> "should_fail" </> "T3102.hs",
    "typecheck" </> "should_fail" </> "T9634.hs",
    "typecheck" </> "should_fail" </> "tcfail089.hs",
    "typecheck" </> "should_run" </> "T1735.hs",
    "typecheck" </> "should_run" </> "T1735_Help" </> "Main.hs",
    "typecheck" </> "should_run" </> "T4809.hs",
    "unboxedsums" </> "UnboxedSumsTH_Fail.hs",
    "unboxedsums" </> "unboxedsums4.hs",
    "warnings" </> "should_fail" </> "CaretDiagnostics2.hs",
    "wcompat-warnings" </> "WCompatWarningsOff.hs",
    "wcompat-warnings" </> "WCompatWarningsOn.hs",
    "wcompat-warnings" </> "WCompatWarningsOnOff.hs"
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
    -- | What the package around it puts in force, already resolved from its
    -- @.cabal@ file. Empty for a corpus whose examples are not modules of a
    -- package; see 'corpusInPackages'.
    exampleExtensions :: [Extension]
  }
  deriving (Eq, Show)

-- | Get a corpus, fetching and unpacking it if this machine does not have
-- it yet.
--
-- Fetching happens once: an unpacked corpus is left in place and found
-- again, and a download interrupted half way leaves nothing behind to be
-- mistaken for a complete one. 'Left' is for the machine that cannot reach
-- the network rather than for a defect, and callers are expected to say so
-- and carry on rather than fail. A vendored corpus is already here and can
-- never fail this way.
obtain :: Corpus -> IO (Either Text [Example])
obtain corpus = case corpusSource corpus of
  Vendored dir -> Right <$> examplesIn corpus dir
  Fetched url root -> do
    home <- corpusCache
    let unpacked = home </> corpusName corpus
    createDirectoryIfMissing True home
    fetch url (home </> corpusName corpus <> ".tar.gz") unpacked >>= \case
      Left problem -> pure (Left problem)
      Right () -> Right <$> examplesIn corpus (unpacked </> root)
  HackageReleases releases -> do
    home <- corpusCache
    let root = home </> corpusName corpus
    createDirectoryIfMissing True root
    inTurn root releases >>= \case
      Left problem -> pure (Left problem)
      Right () -> Right <$> examplesIn corpus root
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
fetch :: (Url 'Https, Option 'Https) -> FilePath -> FilePath -> IO (Either Text ())
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
-- 'Left' is for the machine that cannot reach the network rather than for a
-- defect, and callers are expected to say so and carry on. Only the fetch
-- is caught: a file that cannot be written is a fault worth hearing about.
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
      VanillaHttpException (HTTP.HttpExceptionRequest _ reason) -> flatten reason
      other -> flatten other
    flatten :: (Show a) => a -> Text
    flatten = T.take 200 . T.unwords . T.words . T.pack . show

-- | Unpack the Haskell files of an archive, dropping its top-level
-- directory.
unpackTo :: FilePath -> FilePath -> IO (Either Text ())
unpackTo archive dest = do
  removePathForcibly staging
  outcome <- quietly (Left "could not unpack") $ do
    bytes <- BL.readFile archive
    Tar.foldEntries write (pure ()) (const (pure ())) (Tar.read (GZip.decompress bytes))
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
            any (`isSuffixOf` path) (".cabal" : haskellExtensions) -> do
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

-- | Every example in an unpacked corpus, in a settled order.
examplesIn :: Corpus -> FilePath -> IO [Example]
examplesIn corpus root = do
  found <- sort <$> haskellFilesIn root
  reader <- packageReaderFor corpus
  let present = Set.fromList found
      files = filter (not . skipped) found
      example f reference = Example (nameOf f) f reference <$> reader f
  case corpusReference corpus of
    NoReference -> traverse (`example` Nothing) files
    ReferenceMarked mark ->
      forM (filter (not . answerTo mark present) files) $ \f ->
        if mark `isSuffixOf` stemOf f
          then example f (Just f)
          else do
            let reference = stemOf f <> mark <> extensionOf f
            there <- doesFileExist reference
            example f (if there then Just reference else Nothing)
  where
    nameOf f = fromMaybe f (stripPrefix (root <> "/") f)
    answerTo mark present f = case withoutSuffix mark (stemOf f) of
      Just stem -> Set.member (stem <> extensionOf f) present
      Nothing -> False
    skipped f = any listed (nameOf f : maybeToList (inputFor (nameOf f)))
    listed name = any covers skips
      where
        covers entry = entry == name || (entry <> "/") `isPrefixOf` name
    skips = case corpusExpectations corpus of
      Listed lists -> expectSkip lists
      Recorded _ -> []
    inputFor name = case corpusReference corpus of
      ReferenceMarked mark
        | Just stem <- withoutSuffix mark (stemOf name) ->
            Just (stem <> extensionOf name)
      _ -> Nothing
    withoutSuffix suffix name
      | suffix `isSuffixOf` name = Just (take (length name - length suffix) name)
      | otherwise = Nothing
    stemOf f =
      fromMaybe f (listToMaybe (mapMaybe (`withoutSuffix` f) haskellExtensions))
    extensionOf f =
      fromMaybe "" (listToMaybe (filter (`isSuffixOf` f) haskellExtensions))

-- | What each of a corpus's examples has in force before its own pragmas.
packageReaderFor :: Corpus -> IO (FilePath -> IO [Extension])
packageReaderFor corpus
  | not (corpusInPackages corpus) = pure (const (pure []))
  | otherwise = do
      reader <- newPackageReader
      pure (fmap (either (const []) id) . reader)

-- | The extensions an example may be written with.
haskellExtensions :: [String]
haskellExtensions = [".hs", ".hs-boot", ".hsig"]

haskellFilesIn :: FilePath -> IO [FilePath]
haskellFilesIn dir = do
  isDir <- doesDirectoryExist dir
  if not isDir
    then pure [dir | any (`isSuffixOf` dir) haskellExtensions]
    else do
      entries <- quietly [] (listDirectory dir)
      concat <$> traverse (haskellFilesIn . (dir </>)) entries

----------------------------------------------------------------------------
-- Helpers

quietly :: a -> IO a -> IO a
quietly fallback action =
  try action >>= \case
    Left (_ :: SomeException) -> pure fallback
    Right a -> pure a
