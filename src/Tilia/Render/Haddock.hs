{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Haddocks.
module Tilia.Render.Haddock
  ( DocStyle (..),
    Ending (..),
    haddock,
    haddockInline,
    docSectionName,
    brokenIfDocumented,
    printsWholeLineDocs,
    haddockSpans,
  )
where

import Control.Applicative ((<|>))
import Data.Data (Data)
import Data.Generics.Schemes (listify)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
import GHC.Types.SrcLoc (GenLocated (..), getLoc, unLoc)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Span
import Tilia.Span.Ghc

-- | Which kind of Haddock is being printed.
data DocStyle
  = -- | @-- |@, documenting what follows
    Pipe
  | -- | @-- ^@, documenting what precedes
    Caret
  | -- | @-- *@, a section heading, at the given depth
    Section Int
  | -- | @-- $name@, a named chunk
    Chunk String
  deriving (Eq, Show)

-- | Whether the Haddock ends the line it is on.
data Ending
  = -- | The caller will end the line itself.
    Open
  | -- | End it here.
    Closed
  deriving (Eq, Show)

-- | Print a Haddock.
haddock :: Ctx -> DocStyle -> Ending -> LHsDoc GhcPs -> Doc
haddock ctx style ending doc = fst (docBody ctx style doc) <> close
  where
    close = case ending of
      Open -> mempty
      Closed -> hardBreak

-- | A Haddock inside a construct that may legitimately stay on one line.
--
-- A @{- | … -}@ delimits itself, so @data A = A {- | a number -} Int@ is
-- left as written. A @--@ Haddock owns the rest of its line and still has to
-- end it.
haddockInline :: Ctx -> DocStyle -> LHsDoc GhcPs -> Doc
haddockInline ctx style doc =
  body <> (if isSelfClosing then breakOrSpace else hardBreak)
  where
    (body, isSelfClosing) = docBody ctx style doc

-- | The Haddock itself, and whether the form it took delimits itself.
docBody :: Ctx -> DocStyle -> LHsDoc GhcPs -> (Doc, Bool)
docBody ctx style doc@(L l str) =
  case reusableText ctx style doc of
    Just written ->
      ( maybe id located (spanOfSrcSpan l)
          $ align
          $ sepBy
            (verbatimBreak AtIndent TrimWhitespace)
            (fmap txt (NE.toList written)),
        selfClosing written
      )
    Nothing
      | null written' -> (emptyBlock, True)
      | blockForm -> (rebuiltBlock, False)
      | otherwise -> (rebuilt, False)
  where
    emptyBlock = txt (blockOpener style) <> space <> txt "-}"
    rebuilt =
      sepBy hardBreak (zipWith line' (True : repeat False) written')
        <> mconcat (replicate trailingBlanks (hardBreak <> txt "--"))
    trailingBlanks = case writtenHaddock ctx (spanOfSrcSpan l) of
      Nothing -> 0
      Just ls -> length (takeWhile isBlankLine (reverse (NE.toList ls)))
    isBlankLine t = T.null (T.strip (fromMaybe t (T.stripPrefix "--" (T.strip t))))
    line' isFirst t =
      (if isFirst then txt (opener style) else txt "--")
        <> space
        <> txt t
    rebuiltBlock =
      align $
        txt (blockOpener style)
          <> space
          <> sepBy (verbatimBreak AtIndent TrimWhitespace) (fmap txt written')
          <> space
          <> txt "-}"
    asBlock = writtenAsBlock ctx doc
    written' = docLines asBlock str
    blockForm = asBlock && length written' > 1

-- | How a rebuilt Haddock begins.
opener :: DocStyle -> Text
opener = \case
  Pipe -> "-- |"
  Caret -> "-- ^"
  Section n -> "-- " <> T.replicate n "*"
  Chunk n -> docSectionName n

-- | How a rebuilt Haddock that stays a block comment begins.
blockOpener :: DocStyle -> Text
blockOpener = \case
  Pipe -> "{- |"
  Caret -> "{- ^"
  Section n -> "{- " <> T.replicate n "*"
  Chunk n -> "{- $" <> T.pack n

-- | Did the author write this Haddock as a block comment?
writtenAsBlock :: Ctx -> LHsDoc GhcPs -> Bool
writtenAsBlock ctx doc =
  maybe False isBlockForm (writtenHaddock ctx (spanOfSrcSpan (getLoc doc)))

-- | The anchor of a named documentation chunk.
--
-- Unlike a Haddock this carries no text of its own, so there is nothing to
-- reuse and nothing to report a position for.
docSectionName :: String -> Text
docSectionName n = "-- $" <> T.pack n

-- | The author's own text, when it may be used.
--
-- It may not when the Haddock is about to be printed in a style other than
-- the one it was written in, since the text carries the style in its first
-- characters.
reusableText :: Ctx -> DocStyle -> LHsDoc GhcPs -> Maybe (NonEmpty Text)
reusableText ctx style doc = do
  written <- writtenHaddock ctx (spanOfSrcSpan (getLoc doc))
  if openedInStyle style (NE.head written) then Just written else Nothing

-- | Was the Haddock written in the style it is about to come back out in?
openedInStyle :: DocStyle -> Text -> Bool
openedInStyle style firstLine = case afterOpener firstLine of
  Nothing -> False
  Just inside -> case style of
    Chunk _ -> triggerFor style `T.isPrefixOf` inside
    _ -> triggerOn inside == Just (triggerFor style)

-- | The trigger a style is written with.
triggerFor :: DocStyle -> Text
triggerFor = \case
  Pipe -> "|"
  Caret -> "^"
  Section n -> T.replicate n "*"
  Chunk n -> "$" <> T.pack n

-- | What follows the @--@ or @{-@ that opens a comment, with the spaces
-- after it removed.
afterOpener :: Text -> Maybe Text
afterOpener firstLine = T.stripStart <$> opened (T.stripStart firstLine)
  where
    opened t = T.stripPrefix "--" t <|> T.stripPrefix "{-" t

-- | The trigger an opened comment carries, for the triggers that are a run
-- of one character.
triggerOn :: Text -> Maybe Text
triggerOn inside = do
  (c, rest) <- T.uncons inside
  case c of
    '|' -> Just "|"
    '^' -> Just "^"
    '*' -> Just (T.cons c (T.takeWhile (== '*') rest))
    _ -> Nothing

-- | Was the reused text a block comment?
isBlockForm :: NonEmpty Text -> Bool
isBlockForm written = "{-" `T.isPrefixOf` T.stripStart (NE.head written)

-- | May code follow the reused text on the line it ends?
selfClosing :: NonEmpty Text -> Bool
selfClosing written = isBlockForm written && null (NE.tail written)

-- | Lay the document out on several lines if printing this fragment will
-- emit a Haddock that takes whole lines.
brokenIfDocumented :: (Data a) => Ctx -> a -> Doc -> Doc
brokenIfDocumented ctx x d
  | printsWholeLineDocs ctx x = broken d
  | otherwise = d

-- | Will printing this fragment emit a Haddock as @--@ lines?
--
-- Every site that asks prints in 'Pipe' style, which is what decides
-- whether the author's text can be reused.
printsWholeLineDocs :: (Data a) => Ctx -> a -> Bool
printsWholeLineDocs ctx x = case docsIn x of
  [] -> not (null (docStringsIn x))
  docs -> any takesWholeLines docs
  where
    takesWholeLines doc = case reusableText ctx Pipe doc of
      Just written -> not (selfClosing written)
      Nothing -> not (null (docLines (writtenAsBlock ctx doc) (unLoc doc)))

-- | The spans of every Haddock in a fragment.
haddockSpans :: (Data a) => a -> [Span]
haddockSpans x = mapMaybe (spanOfSrcSpan . getLoc) (docsIn x) <> namedSections x

-- | Every Haddock in a fragment.
docsIn :: (Data a) => a -> [LHsDoc GhcPs]
docsIn = listify (const True :: LHsDoc GhcPs -> Bool)

-- | The spans of the @-- $name@ anchors in an export list.
namedSections :: (Data a) => a -> [Span]
namedSections =
  mapMaybe anchorSpan . listify (const True :: LIE GhcPs -> Bool)
  where
    anchorSpan l = case unLoc l of
      IEDocNamed{} -> spanOfSrcSpan (getHasLoc (getLoc l))
      _ -> Nothing

-- | Every doc string in a fragment, even one no 'LHsDoc' holds.
docStringsIn :: (Data a) => a -> [HsDocString]
docStringsIn = listify (const True :: HsDocString -> Bool)

-- | The lines of a doc string, normalised the way Haddock reads them.
docLines ::
  -- | Was it written as a block comment?
  Bool ->
  WithHsDocIdentifiers HsDocString GhcPs ->
  [Text]
docLines blockForm str
  | null body = []
  | otherwise = fmap guardDollar (dedent (fmap unpad body))
  where
    body =
      dropWhileEnd T.null
        . fmap (T.stripEnd . T.pack)
        . lines
        . renderHsDocString
        $ hsDocString str
    unpad t
      | padded, Just (' ', rest) <- T.uncons t = rest
      | otherwise = t
    padded = case dropWhile T.null body of
      (t : _) -> " " `T.isPrefixOf` t
      [] -> False
    dedent ls
      | not blockForm = ls
      | otherwise = case ls of
          [] -> []
          (first' : rest) -> first' : fmap (T.drop (shared rest)) rest
    shared ls = case fmap indentation (filter (not . T.null) ls) of
      [] -> 0
      ns -> minimum ns
    indentation = T.length . T.takeWhile (== ' ')
    guardDollar t
      | "$" `T.isPrefixOf` t = T.cons '\\' t
      | otherwise = t
