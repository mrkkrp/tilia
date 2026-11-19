{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | String literals.
module Tilia.Render.Literal
  ( hsLit,
    writtenString,
    stringLiteral,
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.List (find)
import Data.Semigroup (Min (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (FastString, unpackFS)
import GHC.Hs (GhcPs, HsLit (..))
import GHC.LanguageExtensions.Type (Extension (Cpp))
import GHC.Parser.CharClass (is_space)
import GHC.Types.SourceText (SourceText (..), StringLiteral (..))

import Tilia.Cpp.Expansion (endsInBackslash)
import Tilia.Doc.Combinators
import Tilia.Render.Context (Ctx, extensionOn)
import Tilia.Render.Layout (Position (..), positioned)
import Tilia.Render.Name (outputable)

-- | A literal, a string one as its author wrote it.
hsLit ::
  Ctx ->
  -- | The literal.
  HsLit GhcPs ->
  Doc
hsLit ctx = \case
  HsString (SourceText s) _ -> stringLiteral ctx s
  HsStringPrim (SourceText s) _ -> stringLiteral ctx s
  HsMultilineString (SourceText s) _ -> stringLiteral ctx s
  other -> outputable other

-- | A string literal as its author wrote it, or the bare text it holds where
-- none was written, as for an unquoted @SCC@ label.
writtenString ::
  Ctx ->
  -- | The literal.
  StringLiteral ->
  Doc
writtenString ctx l = case sl_st l of
  SourceText s -> stringLiteral ctx s
  NoSourceText -> txt (T.pack (unpackFS (sl_fs l)))

-- | A string literal, from the text the author wrote, line for line where
-- cpp joins its lines, since their indentation is then part of the string.
stringLiteral ::
  Ctx ->
  -- | The text the author wrote.
  FastString ->
  Doc
stringLiteral ctx src
  | extensionOn ctx Cpp,
    any endsInBackslash (drop 1 (reverse written)) =
      verbatimLines AtMargin KeepWhitespace written
  | otherwise = case takeApart text of
      Nothing -> error ("Tilia: unparsable string literal: " <> show src)
      Just literal -> align (renderLiteral literal)
  where
    text = T.pack (unpackFS src)
    written = T.splitOn "\n" text

-- | Print a literal back out, on one line or across several.
renderLiteral :: Literal -> Doc
renderLiteral literal =
  txt (litOpen literal) <> body <> txt (litClose literal)
  where
    body = case litKind literal of
      Regular -> variant onOneLine acrossLines
      Multiline -> verbatimLines AtIndent KeepWhitespace (litParts literal)
    onOneLine = txt (joinParts (litParts literal))
    acrossLines =
      sepBy breakOrSpace (fmap continued (positioned (litParts literal)))
    continued (position, s) = case position of
      Only -> txt s
      First -> txt s <> txt "\\"
      Middle -> txt "\\" <> txt s <> txt "\\"
      Last -> txt "\\" <> txt s

-- | A literal split into the bits that may be laid out separately.
data Literal = Literal
  { -- | The opening quote.
    litOpen :: Text,
    -- | The closing quote, with any @#@ that followed it.
    litClose :: Text,
    -- | Which syntax it was written in.
    litKind :: LiteralKind,
    -- | For a regular literal, the runs between string gaps; for a
    -- multi-line one, the lines.
    litParts :: [Text]
  }
  deriving (Eq, Show)

-- | Which of the two string syntaxes a literal was written in.
data LiteralKind
  = Regular
  | Multiline
  deriving (Eq, Show)

-- | Take a literal apart into its quotes and the parts between them.
takeApart :: Text -> Maybe Literal
takeApart s = do
  literal <-
    stripMarkers Multiline "\"\"\"" s
      <|> stripMarkers Regular "\"" s
  let split = case litKind literal of
        Regular -> runsBetweenGaps
        Multiline -> splitMultiline
  pure literal{litParts = concatMap split (litParts literal)}

-- | Peel the quotes off, allowing for the @#@ that marks an unlifted
-- literal.
stripMarkers :: LiteralKind -> Text -> Text -> Maybe Literal
stripMarkers litKind marker s = do
  inner <- T.stripPrefix marker s
  litClose <- find (`T.isSuffixOf` inner) [marker <> "#", marker]
  body <- T.stripSuffix litClose inner
  pure Literal{litOpen = marker, litParts = [body], ..}

-- | The runs of a literal either side of its string gaps.
runsBetweenGaps :: Text -> [Text]
runsBetweenGaps s = case gapAt 0 s of
  Nothing -> [s]
  Just (before, after) -> T.take before s : runsBetweenGaps after
  where
    gapAt n t = case T.uncons t of
      Nothing -> Nothing
      Just ('\\', rest) -> case afterGap rest of
        Just resumes -> Just (n, resumes)
        Nothing ->
          let taken = 1 + escapedWidth rest
           in gapAt (n + taken) (T.drop taken t)
      Just (_, rest) -> gapAt (n + 1) rest
    afterGap t = case T.span is_space t of
      (blank, rest)
        | not (T.null blank),
          Just ('\\', resumes) <- T.uncons rest ->
            Just resumes
      _ -> Nothing
    escapedWidth t = case T.uncons t of
      Just ('^', _) -> 2
      Just _ -> 1
      Nothing -> 0

-- | Split a multi-line literal the way GHC's lexer reads one, so that what
-- comes back out means what went in.
splitMultiline :: Text -> [Text]
splitMultiline =
  dropCommonIndent
    . fmap expandTabs
    . splitLines
    . joinParts
    . runsBetweenGaps

-- | The line terminators the Report recognises, not merely @\\n@.
splitLines :: Text -> [Text]
splitLines = T.splitOn "\r\n" >=> T.split newlineish
  where
    newlineish c = c == '\n' || c == '\r' || c == '\f'

-- | Tabs advance to the next multiple of eight.
expandTabs :: Text -> Text
expandTabs = T.concat . go 0
  where
    go column s = case T.breakOn "\t" s of
      (before, T.uncons -> Just (_, after)) ->
        let reached = column + T.length before
            fill = 8 - (reached `mod` 8)
         in before : T.replicate fill " " : go (reached + fill) after
      _ -> [s]

-- | Take the common indentation off every line but the first, and blank the
-- lines that were nothing but whitespace.
dropCommonIndent :: [Text] -> [Text]
dropCommonIndent = \case
  [] -> []
  firstLine : rest -> firstLine : trimmed
    where
      (indents, trimmed) = unzip (fmap measure rest)
      common = maybe 0 getMin (mconcat indents)
      measure l
        | T.all is_space l = (Nothing, "")
        | otherwise =
            (Just (Min (T.length (T.takeWhile is_space l))), T.drop common l)

-- | Rejoin runs with the smallest gap that keeps them apart.
--
-- The gap cannot simply be dropped: it is what stops the end of one run and
-- the start of the next from lexing as a single escape sequence, so
-- @\"\\65\\ \\0\"@ and @\"\\650\"@ are different strings.
joinParts :: [Text] -> Text
joinParts = T.intercalate "\\ \\"
