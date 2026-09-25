{-# LANGUAGE OverloadedStrings #-}

-- | Formatting whole modules.
module Tilia.RenderSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.LanguageExtensions.Type (Extension (..))
import Test.Hspec
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Fixity
  ( Direction (..),
    Fixity (..),
    OpName (..),
    Provenance (..),
    Reach (..),
    Scope (..),
  )
import Tilia.Parser (defaultParserConfig, parseModule)
import Tilia.Render

spec :: Spec
spec = do
  describe "the module header" $ do
    it "puts the pragmas above the module and sorts them" $
      format
        [ "{-# LANGUAGE OverloadedStrings, GADTs #-}",
          "module M where"
        ]
        `shouldBe` [ "{-# LANGUAGE GADTs #-}",
                     "{-# LANGUAGE OverloadedStrings #-}",
                     "",
                     "module M where"
                   ]

    it "puts an extension pack before what it enables" $
      format
        [ "{-# LANGUAGE ApplicativeDo #-}",
          "{-# LANGUAGE GHC2021 #-}",
          "module M where"
        ]
        `shouldBe` [ "{-# LANGUAGE GHC2021 #-}",
                     "{-# LANGUAGE ApplicativeDo #-}",
                     "",
                     "module M where"
                   ]

    it "puts qualified first throughout when the extension is off" $
      format
        [ "module M where",
          "import Data.Map qualified as M",
          "import qualified Data.Set as S"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "import qualified Data.Map as M",
                     "import qualified Data.Set as S"
                   ]

    it "puts qualified last throughout when the extension is on" $
      formatUnder
        [ImportQualifiedPost]
        [ "module M where",
          "import Data.Map qualified as M",
          "import qualified Data.Set as S"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "import Data.Map qualified as M",
                     "import Data.Set qualified as S"
                   ]

    it "parses a module that takes back an extension the edition puts in force" $
      format
        [ "{-# LANGUAGE NoStarIsType #-}",
          "{-# LANGUAGE TypeOperators #-}",
          "module M (type (*)) where",
          "import GHC.TypeLits (type (*))"
        ]
        `shouldBe` [ "{-# LANGUAGE TypeOperators #-}",
                     "{-# LANGUAGE NoStarIsType #-}",
                     "",
                     "module M (type (*)) where",
                     "",
                     "import GHC.TypeLits (type (*))"
                   ]

  describe "comments" $ do
    it "keeps one written between declarations" $
      format
        [ "module M where",
          "",
          "-- a note",
          "f = 1"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "-- a note",
                     "f = 1"
                   ]

    it "keeps one written inside a binding" $
      format
        [ "module M where",
          "",
          "f = g",
          "  where",
          "    -- about g",
          "    g = 1"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "f = g",
                     "  where",
                     "    -- about g",
                     "    g = 1"
                   ]

    it "keeps a self-delimiting one on the line it was written on" $
      format ["module M where", "", "f =", "  {- here -} 1"]
        `shouldBe` ["module M where", "", "f =", "  {- here -} 1"]

    it "will not put a construct holding one on a single line" $
      format
        [ "module M where",
          "",
          "f =",
          "  ( -- here",
          "    1",
          "  )"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "f =",
                     "  ( -- here",
                     "    1",
                     "  )"
                   ]

  describe "operator chains" $ do
    -- With the fixities known, the chain is regrouped so that what binds
    -- tightly stays together and the break falls where the reader expects.
    it "breaks a chain at its loosest operator" $
      formatWith (Just arithmetic) spreadChain
        `shouldBe` [ "module M where",
                     "",
                     "f =",
                     "  a * b",
                     "    + c * d"
                   ]

    -- Without them nothing is asserted about how the chain associates, so
    -- nothing is rearranged and every operator is treated alike.
    it "leaves a chain alone when the fixities are unknown" $
      formatWith Nothing spreadChain
        `shouldBe` [ "module M where",
                     "",
                     "f =",
                     "  a",
                     "    * b",
                     "    + c",
                     "    * d"
                   ]

    it "lets a separator hand a block to what precedes it" $
      formatWith
        (Just arithmetic)
        ["module M where", "", "f = g $ do", "  h", "  i"]
        `shouldBe` [ "module M where",
                     "",
                     "f = g $ do",
                     "  h",
                     "  i"
                   ]

  describe "where a comment lands" $ do
    -- A comment the author wrote after code ends a line here too, and the
    -- printer has not finished with that line: the comma of a record field,
    -- the arrow of an alternative and the closing bracket of a list all
    -- still have to be emitted, and all of them belong before it.
    it "keeps a comment at the end of the line it was written on" $
      format
        [ "module M where",
          "",
          "f = case x of",
          "  a -> -- why",
          "    b"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "f = case x of",
                     "  a -> -- why",
                     "    b"
                   ]

    it "leaves a comment written on its own line on one" $
      format
        [ "module M where",
          "",
          "f = case x of",
          "  a ->",
          "    -- why",
          "    b"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "f = case x of",
                     "  a ->",
                     "    -- why",
                     "    b"
                   ]

    it "keeps one written before the closing bracket inside it" $
      format
        [ "module M where",
          "",
          "xs =",
          "  [ a,",
          "    b",
          "    -- and that is all",
          "  ]"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "xs =",
                     "  [ a,",
                     "    b",
                     "    -- and that is all",
                     "  ]"
                   ]

  -- Formatting an already formatted file must change nothing. The property
  -- is easy to lose the moment comments move, since where a comment goes is
  -- read off the input and moving it changes what the next pass reads.
  describe "settling"
    $ it "reaches its answer in one pass"
    $ let once = format awkward
       in formatWith Nothing once `shouldBe` once

  describe "layout follows the input" $ do
    it "keeps a declaration that was on one line on one line" $
      format ["module M where", "", "f x = (x, x)"]
        `shouldBe` ["module M where", "", "f x = (x, x)"]

    it "keeps one that was spread out spread out" $
      format ["module M where", "", "f x =", "  ( x,", "    x", "  )"]
        `shouldBe` [ "module M where",
                     "",
                     "f x =",
                     "  ( x,",
                     "    x",
                     "  )"
                   ]

----------------------------------------------------------------------------
-- Helpers

-- | Format the given lines and give the result back as lines.
format :: [Text] -> [Text]
format = formatWith Nothing

-- | Format with the given extensions in force, as a package would put them.
formatUnder :: [Extension] -> [Text] -> [Text]
formatUnder exts = withSettings defaultRenderConfig{rcExtensions = Set.fromList exts}

formatWith :: Maybe Scope -> [Text] -> [Text]
formatWith scope = withSettings defaultRenderConfig{rcScope = scope}

withSettings :: RenderConfig -> [Text] -> [Text]
withSettings settings input =
  case parseModule defaultParserConfig "<test>" source of
    Left _ -> error ("did not parse:\n" <> T.unpack source)
    Right parsed ->
      T.lines (printDoc defaultRenderOptions (renderModule settings parsed))
  where
    source = T.unlines input

-- | A module with comments in all the places that are hard to put them
-- back.
awkward :: [Text]
awkward =
  [ "module M where",
    "",
    "-- | A heading",
    "",
    "-- and a note under it",
    "data T = T",
    "  { a :: Int, -- the first",
    "    -- the second",
    "    b :: Int",
    "  }",
    "",
    "f x -- what to do with this?",
    "  | x > 0 = g x",
    "  | otherwise = h x",
    "  where",
    "    g = id",
    "",
    "    -- about h",
    "    h = negate",
    "",
    "xs =",
    "  [ one,",
    "    {- inline -} two",
    "    -- last",
    "  ]"
  ]

-- | A chain the author already spread over more than one line, so that the
-- question is where it breaks rather than whether it does.
spreadChain :: [Text]
spreadChain =
  [ "module M where",
    "",
    "f =",
    "  a * b",
    "    + c * d"
  ]

-- | A scope that knows the operators the tests use.
arithmetic :: Scope
arithmetic =
  Scope
    { scopeInTypes = nothingReaches,
      scopeInTerms =
        nothingReaches
          { reachUnqualified =
              Map.fromList
                [ (OpName "$", (Fixity RightAssoc 0, DeclaredHere)),
                  (OpName "+", (Fixity LeftAssoc 6, DeclaredHere)),
                  (OpName "*", (Fixity LeftAssoc 7, DeclaredHere))
                ]
          },
      scopeUnread = []
    }

-- | A namespace with nothing in it.
nothingReaches :: Reach
nothingReaches =
  Reach
    { reachUnqualified = Map.empty,
      reachQualified = Map.empty,
      reachAmbiguous = []
    }
