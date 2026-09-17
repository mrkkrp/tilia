{-# LANGUAGE OverloadedStrings #-}

-- | Extraction of the comment stream from real source text, and the
-- normalizations applied on the way.
module Tilia.CommentsSpec (spec) where

import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Comments
import Tilia.Comments.Attach
import Tilia.Doc
import Tilia.Doc.Combinators
import Tilia.Parser
import Tilia.Source (comments)
import Tilia.Span

spec :: Spec
spec = do
  describe "extraction" $ do
    it "finds a comment on its own line" $
      bodies "module M where\n-- a comment\nx = 1\n"
        `shouldBe` [["-- a comment"]]

    it "finds several, in source order" $
      bodies "module M where\n-- one\nx = 1\n-- two\ny = 2\n"
        `shouldBe` [["-- one"], ["-- two"]]

    it "finds a block comment and keeps its lines" $
      bodies "module M where\n{- one\n   two -}\nx = 1\n"
        `shouldBe` [["{- one", "   two -}"]]

    it "reports no comments when there are none" $
      bodies "module M where\nx = 1\n" `shouldBe` []

  describe "trailing" $ do
    it "marks a comment that follows code on its line" $
      trailings "module M where\nx = 1 -- here\n" `shouldBe` [True]
    it "does not mark one on a line of its own" $
      trailings "module M where\n-- here\nx = 1\n" `shouldBe` [False]
    it "does not mark one indented on a line of its own" $
      trailings "module M where\nx =\n    -- here\n    1\n" `shouldBe` [False]

  -- The lexer counts a tab as advancing to the next multiple of eight, so a
  -- line holding one has more columns than characters. Everything here works
  -- by cutting the source at a column the compiler reported, and cutting at
  -- the wrong place is not a crash but a comment that quietly believes it
  -- has nothing after it.
  describe "lines indented with tabs" $ do
    it "sees the code before a comment" $
      trailings "module M where\n\tx = 1 -- here\n" `shouldBe` [True]

    it "sees that a comment has the line to itself" $
      trailings "module M where\n\t-- here\n\tx = 1\n" `shouldBe` [False]

    it "sees the code after a block comment" $
      followeds "module M where\n\tx = f {- here -} 1\n" `shouldBe` [True]

    it "sees that nothing follows a block comment" $
      followeds "module M where\n\tx = f 1 {- here -}\n" `shouldBe` [False]

    it "takes the comment's text and no more" $
      bodies "module M where\n\tx = f {- here -} 1\n" `shouldBe` [["{- here -}"]]

    it "dedents a block comment by what precedes it" $
      bodies "module M where\n\t{- one\n\t   two -}\nx = 1\n"
        `shouldBe` [["{- one", "   two -}"]]

  describe "normalization: space after dashes" $ do
    it "adds a missing space" $
      bodies "module M where\n--tight\nx = 1\n" `shouldBe` [["-- tight"]]
    it "leaves an existing space alone" $
      bodies "module M where\n-- loose\nx = 1\n" `shouldBe` [["-- loose"]]
    it "leaves a divider alone" $
      bodies "module M where\n-------\nx = 1\n" `shouldBe` [["-------"]]
    it "does not touch dashes inside a block comment" $
      bodies "module M where\n{--tight-}\nx = 1\n" `shouldBe` [["{--tight-}"]]

  describe "normalization: trailing whitespace"
    $ it "strips it from every line"
    $ bodies "module M where\n{- one   \n   two   \n   three -}\nx = 1\n"
      `shouldBe` [["{- one", "   two", "   three -}"]]

  describe "normalization: dedent" $ do
    it "drops the comment\'s own start column, not all indentation" $
      bodies "module M where\nx =\n  {- one\n     two\n     three -}\n  1\n"
        `shouldBe` [["{- one", "   two", "   three -}"]]
    it "keeps relative indentation between continuation lines" $
      bodies "module M where\nx =\n  {- one\n     two\n       three -}\n  1\n"
        `shouldBe` [["{- one", "   two", "     three -}"]]
    it "leaves an unindented comment alone" $
      bodies "module M where\n{- one\n     two -}\nx = 1\n"
        `shouldBe` [["{- one", "     two -}"]]

  -- Widening is not part of extraction: whether a doc comment's trigger is
  -- tidied or escaped depends on whether the syntax tree turned out to
  -- carry it, which nothing here knows. So it is asked for.
  describe "normalization: doc trigger" $ do
    it "widens a tight trigger" $
      widened "module M where\n-- |Foo\nx = 1\n" `shouldBe` [["-- | Foo"]]
    it "leaves an already spaced trigger alone" $
      widened "module M where\n-- | Foo\nx = 1\n" `shouldBe` [["-- | Foo"]]
    it "widens a caret trigger" $
      widened "module M where\nx = 1\n-- ^Foo\n" `shouldBe` [["-- ^ Foo"]]
    it "widens a section trigger, keeping its stars" $
      widened "module M where\n-- **Foo\nx = 1\n" `shouldBe` [["-- ** Foo"]]
    it "leaves a named anchor alone" $
      widened "module M where\n-- $section\nx = 1\n" `shouldBe` [["-- $section"]]
    it "leaves a trigger with nothing after it alone" $
      widened "module M where\n-- |\nx = 1\n" `shouldBe` [["-- |"]]
    it "shifts continuation lines to match" $
      widened "module M where\n{-|Foo\n  bar\n-}\nx = 1\n"
        `shouldBe` [["{-| Foo", "   bar", " -}"]]
    it "does not widen an ordinary line comment" $
      widened "module M where\n-- x|y\nz = 1\n" `shouldBe` [["-- x|y"]]

  describe "pragmas" $ do
    it "recognises one" $
      (commentPragma <$> commentsIn "{-# LANGUAGE CPP #-}\nmodule M where\nx = 1\n")
        `shouldBe` [Just (Pragma "LANGUAGE" "CPP")]
    it "upper-cases the name" $
      (fmap pragmaName . commentPragma <$> commentsIn "{-# language CPP #-}\nmodule M where\nx = 1\n")
        `shouldBe` [Just "LANGUAGE"]
    it "is not fooled by an ordinary block comment" $
      (commentPragma <$> commentsIn "module M where\n{- not a pragma -}\nx = 1\n")
        `shouldBe` [Nothing]
    it "reports the header boundary" $
      headerLine "{-# LANGUAGE CPP #-}\nmodule M where\nimport Data.List\nx = 1\n"
        `shouldBe` Just 3
    it "reports no boundary for a module with only a header" $
      headerLine "{-# LANGUAGE CPP #-}\nmodule M where\n" `shouldBe` Nothing

  describe "what is deliberately not normalized" $ do
    it "keeps blank lines inside a comment" $
      bodies "module M where\n{- one\n\n\n   two -}\nx = 1\n"
        `shouldBe` [["{- one", "", "", "   two -}"]]
    it "does not escape a Haddock trigger" $
      bodies "module M where\nx = 1\n\n-- | not attached to anything\n"
        `shouldBe` [["-- | not attached to anything"]]

  describe "renderComment"
    $ it "joins the lines back with newlines"
    $ renderComment <$> commentsIn "module M where\n{- one\n   two -}\nx = 1\n"
      `shouldBe` ["{- one\n   two -}"]

  describe "attachment" $ do
    it "puts a comment before the node it precedes" $
      let c = one "module M where\n-- note\nx = 1\n"
          d = located (mkSpan (3, 1) (3, 5)) (txt "x = 1")
       in render (attachComments [c] d) `shouldBe` "-- note\nx = 1\n"

    it "keeps a trailing comment on the same line" $
      let c = one "module M where\nx = 1 -- note\n"
          d = located (mkSpan (2, 1) (2, 6)) (txt "x = 1")
       in render (attachComments [c] d) `shouldBe` "x = 1 -- note\n"

    it "descends into the node that contains the comment" $
      let c = one "module M where\nx =\n  -- note\n  1\n"
          inner = located (mkSpan (4, 3) (4, 4)) (txt "1")
          d = located (mkSpan (2, 1) (4, 4)) (txt "x =" <> indent (hardBreak <> inner))
       in render (attachComments [c] d) `shouldBe` "x =\n  -- note\n  1\n"

    it "appends a comment that follows every node rather than dropping it" $
      let c = one "module M where\nx = 1\n-- after\n"
          d = located (mkSpan (2, 1) (2, 6)) (txt "x = 1")
       in
          -- The blank line is added: a comment after everything is about the
          -- file rather than about the line it happens to follow.
          render (attachComments [c] d) `shouldBe` "x = 1\n\n-- after\n"

    it "reaches inside a variant, whichever branch renders" $
      let c = one "module M where\nx = 1 -- note\n"
          n = located (mkSpan (2, 1) (2, 6)) (txt "x = 1")
          d = variant n (txt "(" <> n <> txt ")")
       in ( countOf "-- note" (render (flat (attachComments [c] d))),
            countOf "-- note" (render (broken (attachComments [c] d)))
          )
            `shouldBe` (1, 1)

    it "places a comment inside an empty construct" $
      let c = one "module M where\nx = [ -- note\n  ]\n"
          d =
            located (mkSpan (2, 5) (3, 4)) $
              txt "[" <> emptyAnchor (mkSpan (3, 4) (3, 4)) <> txt "]"
       in render (attachComments [c] d) `shouldBe` "[ -- note\n]\n"

    it "emits every comment exactly once" $
      let cs = commentsIn "module M where\n-- a\nx = 1 -- b\n-- c\ny = 2\n"
          d = located (mkSpan (3, 1) (5, 6)) (txt "code")
          out = render (attachComments cs d)
       in (length cs, countOf "-- a" out, countOf "-- b" out, countOf "-- c" out)
            `shouldBe` (3, 1, 1, 1)

----------------------------------------------------------------------------
-- Helpers

render :: Doc -> Text
render = printDoc defaultRenderOptions

one :: Text -> Comment
one src = case commentsIn src of
  (c : _) -> c
  [] -> error "the test input had no comments"

countOf :: Text -> Text -> Int
countOf needle = length . T.breakOnAll needle

commentsIn :: Text -> [Comment]
commentsIn src =
  case parseModule defaultParserConfig "test.hs" src of
    Left _ -> error "the test input did not parse"
    Right pm -> comments (pmSource pm)

bodies :: Text -> [[Text]]
bodies = fmap (NE.toList . commentBody) . commentsIn

-- | The bodies a doc comment comes out with once its trigger is tidied.
widened :: Text -> [[Text]]
widened = fmap (NE.toList . commentBody . widenTrigger) . commentsIn

trailings :: Text -> [Bool]
trailings = fmap commentTrailing . commentsIn

followeds :: Text -> [Bool]
followeds = fmap commentFollowed . commentsIn

headerLine :: Text -> Maybe Int
headerLine src = case parseModule defaultParserConfig "test.hs" src of
  Left _ -> error "the test input did not parse"
  Right pm -> spanStartLine <$> pmHeaderEnd pm
