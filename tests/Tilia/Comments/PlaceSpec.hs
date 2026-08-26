{-# LANGUAGE OverloadedStrings #-}

-- | Which region each comment is given to, and which side of it.
module Tilia.Comments.PlaceSpec (spec) where

import Data.Text (Text)
import Test.Hspec
import Tilia.Comments (Comment (..), renderComment)
import Tilia.Comments.Place
import Tilia.Parser
import Tilia.Span

spec :: Spec
spec = do
  describe "a comment written after code" $ do
    it "goes to the region that ends where that code stops" $
      placedIn [foo, theOne] [] "foo = 1 -- note\n"
        `shouldBe` [("-- note", Just (After, theOne))]

    it "goes to the nearest region before it when it ends its line" $
      placedIn [foo] [] "foo = 1 -- note\n"
        `shouldBe` [("-- note", Just (After, foo))]

    it "is left to what follows when code follows it too" $
      placedIn [foo, lastOfLineOne] [] "foo = {- note -} 1\n"
        `shouldBe` [("{- note -}", Just (Before, lastOfLineOne))]

    it "is taken back even so when it was written against a region" $
      placedIn [foo, upToTheEquals, lastOfLineOne] [] "foo = {- note -} 1\n"
        `shouldBe` [("{- note -}", Just (After, upToTheEquals))]

  describe "choosing between the regions on a line" $ do
    it "takes the one ending latest" $
      placedIn [foo, theOne] [] "foo = 1 -- note\n"
        `shouldBe` [("-- note", Just (After, theOne))]

    it "takes the outermost of those ending together" $
      placedIn [theOne, wholeOfLineOne] [] "foo = 1 -- note\n"
        `shouldBe` [("-- note", Just (After, wholeOfLineOne))]

    it "passes over one that has not ended when the comment begins" $
      placedIn [foo, pastTheComment] [] "foo = 1 -- note\n"
        `shouldBe` [("-- note", Just (After, foo))]

  describe "a region the comment was not written inside" $ do
    it "may not have it" $
      placedIn [foo, bar, rhsOfLineOne] [] "foo = 1 -- note\nbar = 2\n"
        `shouldBe` [("-- note", Just (Before, bar))]

    it "may when the comment is inside it too" $
      placedIn [foo, bar] [] "foo = 1 -- note\nbar = 2\n"
        `shouldBe` [("-- note", Just (After, foo))]

  describe "a fence" $ do
    it "keeps a comment printed in place from crossing it" $
      placedIn [foo, bar] [rhsOfLineOne] "foo = 1 {- note -}\nbar = 2\n"
        `shouldBe` [("{- note -}", Just (Before, bar))]

    it "leaves the same comment alone when there is no fence" $
      placedIn [foo, bar] [] "foo = 1 {- note -}\nbar = 2\n"
        `shouldBe` [("{- note -}", Just (After, foo))]

    it "says nothing about a comment held back to the end of a line" $
      placedIn [foo, bar] [rhsOfLineOne] "foo = 1 -- note\nbar = 2\n"
        `shouldBe` [("-- note", Just (After, foo))]

  describe "a comment carrying on a remark from the line above" $ do
    it "goes where that remark went" $
      placedIn [operand, bar'] [] carriedOn
        `shouldBe` [ ("-- said once", Just (After, operand)),
                     ("-- and again", Just (After, operand))
                   ]

    it "does not when it is not lined up with that line" $
      placedIn [operand, bar'] [] indentedFurther
        `shouldBe` [ ("-- said once", Just (After, operand)),
                     ("-- and again", Just (Before, bar'))
                   ]

    it "does not when that line ended in code" $
      placedIn [operand, bar'] [] nothingAbove
        `shouldBe` [("-- and again", Just (Before, bar'))]

    it "does not when what follows lines up with it as well" $
      placedIn [operand, continuation] [] carriedOnThenMore
        `shouldBe` [ ("-- said once", Just (After, operand)),
                     ("-- and again", Just (Before, continuation))
                   ]

  describe "a comment with nothing written against it" $ do
    it "goes above the region that starts first after it" $
      placedIn [bar, laterStill] [] "-- note\nbar = 2\n"
        `shouldBe` [("-- note", Just (Before, bar))]

    it "goes above the outermost of those starting together" $
      placedIn [bar, wholeOfLineTwo] [] "-- note\nbar = 2\n"
        `shouldBe` [("-- note", Just (Before, wholeOfLineTwo))]

    it "goes nowhere at all when nothing follows it" $
      placedIn [foo] [] "foo = 1\n-- note\n"
        `shouldBe` [("-- note", Nothing)]

  describe "what a comment will look like" $ do
    it "sits in the line before a region when code was written after it" $
      shapeOf Before (firstComment "foo = {- note -} 1\n") `shouldBe` InPlace

    it "ends the line before a region when it trailed something" $
      shapeOf Before (firstComment "foo = 1 -- note\n") `shouldBe` EndsTheLine

    it "takes lines of its own before a region otherwise" $
      shapeOf Before (firstComment "-- note\nfoo = 1\n") `shouldBe` OnItsOwnLines

    it "sits in the line after a region when it closes itself" $
      shapeOf After (firstComment "foo = 1 {- note -}\n") `shouldBe` InPlace

    it "is held back after a region when it is one line of dashes" $
      shapeOf After (firstComment "foo = 1 -- note\n") `shouldBe` HeldBack

    it "ends the line after a region when it runs over several" $
      shapeOf After (firstComment "foo = 1 {- one\ntwo -}\n") `shouldBe` EndsTheLine

----------------------------------------------------------------------------
-- The regions the snippets are placed against

-- | @foo@, and the @1@ it is bound to, in @foo = 1@.
foo, theOne :: Span
foo = mkSpan (1, 1) (1, 4)
theOne = mkSpan (1, 7) (1, 8)

-- | Everything on the first line, ending where @theOne@ does.
wholeOfLineOne :: Span
wholeOfLineOne = mkSpan (1, 1) (1, 8)

-- | Up to and including the @=@ of @foo = {- note -} 1@, which is where the
-- code before that comment stops.
upToTheEquals :: Span
upToTheEquals = mkSpan (1, 5) (1, 6)

-- | The @1@ at the end of @foo = {- note -} 1@.
lastOfLineOne :: Span
lastOfLineOne = mkSpan (1, 18) (1, 19)

-- | A region that has not finished by the time the comment starts.
pastTheComment :: Span
pastTheComment = mkSpan (1, 1) (1, 16)

-- | Everything after the @=@ of the first line, comment included.
--
-- Wide enough to hold the comment and narrow enough to leave 'foo' outside
-- it, which is what it takes to keep the two apart.
rhsOfLineOne :: Span
rhsOfLineOne = mkSpan (1, 5) (1, 20)

-- | @bar@ on the second line, and everything on that line.
bar, wholeOfLineTwo :: Span
bar = mkSpan (2, 1) (2, 4)
wholeOfLineTwo = mkSpan (2, 1) (2, 8)

-- | A region further down than anything a test needs.
laterStill :: Span
laterStill = mkSpan (9, 1) (9, 4)

-- | The @a + b@ of the snippets below, and the @bar@ under them.
operand, bar' :: Span
operand = mkSpan (2, 3) (2, 8)
bar' = mkSpan (4, 1) (4, 4)

-- | The @+ c@ that carries the expression on, lined up with the comment.
continuation :: Span
continuation = mkSpan (4, 3) (4, 6)

----------------------------------------------------------------------------
-- The snippets that take more than one line

carriedOn, indentedFurther, nothingAbove, carriedOnThenMore :: Text
carriedOn =
  "foo =\n\
  \  a + b -- said once\n\
  \  -- and again\n\
  \bar = 2\n"
indentedFurther =
  "foo =\n\
  \  a + b -- said once\n\
  \   -- and again\n\
  \bar = 2\n"
nothingAbove =
  "foo =\n\
  \  a + b\n\
  \  -- and again\n\
  \bar = 2\n"
carriedOnThenMore =
  "foo =\n\
  \  a + b -- said once\n\
  \  -- and again\n\
  \  + c\n"

----------------------------------------------------------------------------
-- Running the rules

-- | Where each comment of a snippet was put, in the order they were
-- written.
--
-- 'Nothing' is a comment nothing came for, which the printer writes after
-- the whole document.
placedIn ::
  -- | The regions a comment may be given to
  [Span] ->
  -- | The boundaries a comment printed in place may not be carried across
  [Span] ->
  -- | A module for the comments to be read out of
  Text ->
  [(Text, Maybe (Position, Span))]
placedIn regions fences src =
  [(renderComment c, lookup (commentSpan c) gathered) | c <- cs]
  where
    cs = commentsIn src
    gathered = fst (foldl collect ([], placeComments regions fences cs) regions)
    collect (found, placements) r = case takePlaced r placements of
      (mine, rest) -> (found <> [(commentSpan c, (p, r)) | (p, c) <- mine], rest)

firstComment :: Text -> Comment
firstComment src = case commentsIn src of
  (c : _) -> c
  [] -> error "the test input had no comments"

commentsIn :: Text -> [Comment]
commentsIn src = case parseModule defaultParserConfig "test.hs" src of
  Left _ -> error "the test input did not parse"
  Right pm -> pmComments pm
