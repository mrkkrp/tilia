{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Haddocks.
module Tilia.Render.Haddock
  ( DocStyle (..),
    Ending (..),
    haddock,
    haddockInline,
    blockHaddock,
    docSectionName,
    brokenIfDocumented,
    printsWholeLineDocs,
    haddockSpans,
  )
where

import Data.Char (isSpace)
import Data.Choice (Choice, isTrue, pattern Is, pattern Isn't)
import Data.Data (Data)
import Data.Generics.Schemes (listify)
import Data.List (dropWhileEnd)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (Cpp))
import GHC.Types.SrcLoc (GenLocated (..), getLoc, unLoc)

import Tilia.Comments
  ( Comment (..),
    afterOpener,
    asOrdinary,
    bracketed,
    singleLine,
    triggerOn,
  )
import Tilia.Comments.Attach (Margin (..), commentText)
import Tilia.Doc.Combinators
import Tilia.Gathered (Gathered (..))
import Tilia.Render.Context
import Tilia.Source (isDirective, sourceLines)
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
haddock ctx style ending doc =
  fst (docBody ctx (Isn't #block) style doc) <> close
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
    (body, isSelfClosing) = docBody ctx (Isn't #block) style doc

-- | A Haddock as a block comment, which ends where it closes, so that a
-- comment after it on its line stays out of it.
blockHaddock ::
  -- | The context.
  Ctx ->
  -- | The style to print it in.
  DocStyle ->
  -- | The Haddock.
  LHsDoc GhcPs ->
  Doc
blockHaddock ctx style doc = fst (docBody ctx (Is #block) style doc)

-- | The Haddock itself, and whether the form it took delimits itself.
--
-- One written as @--@ lines that say nothing but the spaces their ends
-- lose is no Haddock to GHC on the next pass, so it comes out as the
-- comment that pass reads.
docBody ::
  -- | The context.
  Ctx ->
  -- | Must it come out as a block comment?
  Choice "block" ->
  -- | The style to print it in.
  DocStyle ->
  -- | The Haddock.
  LHsDoc GhcPs ->
  (Doc, Bool)
docBody ctx block style doc@(L l str)
  | not (isTrue block),
    all isSpace text,
    any (/= '\n') text,
    Just c <- writtenHaddock ctx (spanOfSrcSpan l),
    not (bracketed c || singleLine c) =
      ( writtenAt . sepBy hardBreak . fmap (commentText margin) $
          asOrdinary (sourceLines (ctxSource ctx)) c,
        False
      )
  | otherwise = case reusableText ctx style doc of
      Just c
        | not (isTrue block) || bracketed c ->
            ( maybe id located (spanOfSrcSpan l) (commentText margin c),
              selfClosing c
            )
      _
        | null written' -> (writtenAt emptyBlock, True)
        | blockForm -> (writtenAt rebuiltBlock, False)
        | otherwise -> (writtenAt rebuilt, False)
  where
    text = renderHsDocString (hsDocString str)
    margin = Margin directive (const False)
    directive t = extensionOn ctx Cpp && isDirective t
    writtenAt = fence (spanOfSrcSpan l)
    emptyBlock = txt (blockOpener style) <> space <> txt "-}"
    rebuilt =
      sepBy hardBreak (zipWith line' (True : repeat False) written')
        <> mconcat (replicate trailingBlanks (hardBreak <> txt "--"))
    trailingBlanks = case writtenHaddock ctx (spanOfSrcSpan l) of
      Nothing -> 0
      Just c ->
        length (takeWhile isBlankLine (reverse (NE.toList (commentBody c))))
    isBlankLine t =
      T.null (T.strip (fromMaybe t (T.stripPrefix "--" (T.strip t))))
    line' isFirst t
      | isFirst = txt (opener style) <> space <> txt t
      | "$" `T.isPrefixOf` t = txt "--" <> txt t
      | otherwise = txt "--" <> space <> txt t
    rebuiltBlock =
      align $
        txt (blockOpener style)
          <> space
          <> verbatimLines AtIndent TrimWhitespace written'
          <> space
          <> txt "-}"
    asBlock = writtenAsBlock ctx doc
    written' = docLines asBlock str
    blockForm = isTrue block || asBlock && length written' > 1

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
  maybe False bracketed (writtenHaddock ctx (spanOfSrcSpan (getLoc doc)))

-- | The anchor of a named documentation chunk.
docSectionName :: String -> Text
docSectionName n = "-- $" <> T.pack n

-- | The author's own text, when it may be used.
--
-- It may not when the Haddock is about to be printed in a style other than
-- the one it was written in, since the text carries the style in its first
-- characters, nor when it holds nothing but the spaces the end of a line
-- loses.
reusableText :: Ctx -> DocStyle -> LHsDoc GhcPs -> Maybe Comment
reusableText ctx style doc = do
  c <- writtenHaddock ctx (spanOfSrcSpan (getLoc doc))
  if openedInStyle style (NE.head (commentBody c)) && not onlySpaces
    then Just c
    else Nothing
  where
    onlySpaces = not (null text) && all isSpace text && '\n' `notElem` text
    text = renderHsDocString (hsDocString (unLoc doc))

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

-- | May code follow a Haddock on the line it ends?
selfClosing :: Comment -> Bool
selfClosing c = bracketed c && singleLine c

-- | Lay the document out on several lines if printing this fragment will
-- emit a Haddock that takes whole lines.
brokenIfDocumented ::
  (Data a) =>
  -- | The context.
  Ctx ->
  -- | Does the site put its Haddocks under what they document?
  Choice "under" ->
  -- | The fragment.
  a ->
  -- | Its document.
  Doc ->
  Doc
brokenIfDocumented ctx under x d
  | printsWholeLineDocs ctx under x = broken d
  | otherwise = d

-- | Will printing this fragment emit a Haddock that takes whole lines: any
-- Haddock where the site puts them under what they document, and one
-- printed as @--@ lines where it puts them in front?
--
-- Every site that puts them in front prints in 'Pipe' style, which is what
-- decides whether the author's text can be reused.
printsWholeLineDocs ::
  (Data a) =>
  -- | The context.
  Ctx ->
  -- | Does the site put its Haddocks under what they document?
  Choice "under" ->
  -- | The fragment.
  a ->
  Bool
printsWholeLineDocs ctx under x = case docsIn x of
  [] -> not (null (docStringsIn x))
  docs -> isTrue under || any takesWholeLines docs
  where
    takesWholeLines doc = case reusableText ctx Pipe doc of
      Just c -> not (selfClosing c)
      Nothing -> not (null (docLines (writtenAsBlock ctx doc) (unLoc doc)))

-- | The spans of every Haddock in a module.
haddockSpans :: Gathered -> [Span]
haddockSpans found =
  mapMaybe (spanOfSrcSpan . getLoc) (gatheredDocs found)
    <> namedSections (gatheredEntries found)

-- | Every Haddock in a fragment.
docsIn :: (Data a) => a -> [LHsDoc GhcPs]
docsIn = listify (const True :: LHsDoc GhcPs -> Bool)

-- | The spans of the @-- $name@ anchors among the entries of an export
-- list.
namedSections :: [LIE GhcPs] -> [Span]
namedSections = mapMaybe anchorSpan
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
  | otherwise = dedent (fmap unpad body)
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
