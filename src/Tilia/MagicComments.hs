{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The magic comments @{- TILIA_OFF -}@ and @{- TILIA_ON -}@, and the
-- lines they leave as written.
module Tilia.MagicComments
  ( MagicComment (..),
    magicCommentOf,
    preservedLines,
    turnedOffThroughout,
    Misplacement (..),
    misplacedMagicComment,
  )
where

import Data.Foldable (asum)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust, listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule (..))

import Tilia.Comments (Comment (..), CommentStyle (..), commentTrailing)
import Tilia.Parser (ParsedModule (..))
import Tilia.Source (Source, comments, directiveOnLine, lineTexts, sourceLines)
import Tilia.Span
import Tilia.Span.Ghc (spanOf)

-- | A comment that turns formatting off or back on.
data MagicComment
  = -- | @{- TILIA_OFF -}@.
    TiliaOff
  | -- | @{- TILIA_ON -}@.
    TiliaOn
  deriving (Eq, Show)

-- | The magic comment a comment is, if it is one.
magicCommentOf ::
  -- | The comment.
  Comment ->
  Maybe MagicComment
magicCommentOf c
  | BlockComment <- commentStyle c,
    body :| [] <- commentBody c =
      magicCommentWritten body
  | otherwise = Nothing

-- | The magic comment some text spells, if it spells one.
magicCommentWritten ::
  -- | The text.
  Text ->
  Maybe MagicComment
magicCommentWritten written = case T.strip <$> inside (T.strip written) of
  Just "TILIA_OFF" -> Just TiliaOff
  Just "TILIA_ON" -> Just TiliaOn
  _ -> Nothing
  where
    inside t = T.stripPrefix "{-" =<< T.stripSuffix "-}" t

-- | The lines each @{- TILIA_OFF -}@ leaves as written, from its own line
-- to that of the next @{- TILIA_ON -}@, or to the last line of the module.
preservedLines ::
  -- | The module.
  Source ->
  [(Int, Int)]
preservedLines source = go (mapMaybe marked (comments source))
  where
    marked c = (,spanStartLine (commentSpan c)) <$> magicCommentOf c
    lastLine = length (lineTexts (sourceLines source))
    go = \case
      [] -> []
      (TiliaOff, from) : rest -> case dropWhile ((/= TiliaOn) . fst) rest of
        (_, to) : rest' -> (from, to) : go rest'
        [] -> [(from, lastLine)]
      (TiliaOn, _) : rest -> go rest

-- | Does a @{- TILIA_OFF -}@ above all of a module's code, with no
-- @{- TILIA_ON -}@ after it, leave the whole module as written?
--
-- Read from the text alone, since a module turned off may be one that
-- cannot be parsed or formatted.
turnedOffThroughout ::
  -- | The module as written.
  Text ->
  Bool
turnedOffThroughout written
  | not ("TILIA_OFF" `T.isInfixOf` written) = False
  | otherwise = case break ((== Just TiliaOff) . magicCommentWritten) ls of
      (above, _ : below) ->
        all codeless (zip (scanl deeper 0 above) above)
          && notElem (Just TiliaOn) (fmap magicCommentWritten below)
      _ -> False
  where
    ls = T.lines written
    deeper :: Int -> Text -> Int
    deeper depth l = max 0 (depth + T.count "{-" l - T.count "-}" l)
    codeless (depth, l) =
      depth > 0 || T.null t || any (`T.isPrefixOf` t) ["--", "{-", "#"]
      where
        t = T.strip l

-- | Why a magic comment is not honoured.
data Misplacement
  = -- | It shares its line with code, or does not stand between top-level
    -- declarations.
    NotBetweenDeclarations
  | -- | The lines it leaves as written hold a directive.
    AroundDirective
  deriving (Eq, Show)

-- | The first magic comment that is not honoured, by its line, with why,
-- in the first configuration that has one.
misplacedMagicComment ::
  -- | The module as written, directives and all.
  Text ->
  -- | Its configurations, parsed.
  NonEmpty ParsedModule ->
  Maybe (Int, Misplacement)
misplacedMagicComment written configurations
  | not ("TILIA_O" `T.isInfixOf` written) = Nothing
  | otherwise = asum (fmap misplacedIn configurations)
  where
    ls = T.lines written
    between from to = take (to - from + 1) . drop (from - 1)
    misplacedIn parsed = listToMaybe (sortOn fst (outside <> directives))
      where
        source = pmSource parsed
        hsMod = pmModule parsed
        outside =
          [ (n, NotBetweenDeclarations)
          | c <- comments source,
            isJust (magicCommentOf c),
            let n = spanStartLine (commentSpan c),
            commentTrailing c
              || commentFollowed c
              || n <= headerEnd
              || any
                (\s -> spanStartLine s <= n && n <= spanEndLine s)
                declared
          ]
        directives =
          [ (from, AroundDirective)
          | (from, to) <- preservedLines source,
            any (isJust . directiveOnLine) (between from to ls)
          ]
        declared = mapMaybe spanOf (hsmodDecls hsMod)
        headerEnd =
          maximum $
            maybe 0 (subtract 1 . spanStartLine) (pmHeaderEnd parsed)
              : fmap
                spanEndLine
                ( maybeToList (spanOf =<< hsmodName hsMod)
                    <> maybeToList (spanOf =<< hsmodExports hsMod)
                    <> mapMaybe spanOf (hsmodImports hsMod)
                )
