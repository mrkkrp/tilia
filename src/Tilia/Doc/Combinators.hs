{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The vocabulary for writing printing code.
module Tilia.Doc.Combinators
  ( -- * Documents
    Doc,

    -- * Atoms
    txt,
    space,
    breakOrSpace,
    breakOrNothing,
    hardBreak,
    blankLine,
    LineStart (..),
    TrailingWhitespace (..),
    verbatimLines,
    verbatim,
    emptyAnchor,
    insideBrackets,
    declarationsStart,

    -- * Layout
    Layout (..),
    flat,
    broken,
    frangible,
    infrangible,
    variant,
    located,
    unclaimed,
    fence,
    cppChoice,

    -- * Attachment
    Placement (..),
    attach,

    -- * Indentation
    nest,
    indent,
    align,

    -- * Combining
    sepBy,
    joinedBy,

    -- * Wrapping
    ClosingIndent (..),
    parens,
    bracketWith,
    commaBracketWith,
    spacedBracket,
    backticks,

    -- * Punctuation
    CommaStyle (..),
    comma,
    separatingComma,
    commaSep,
    hangingCommas,
    commaSepUnderFirst,
    commaSepAfter,

    -- * Conditionals
    includeWhen,
    includeUnless,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T

import Tilia.Doc
  ( Conditional (..),
    Doc (..),
    Layout (..),
    LineStart (..),
    Nesting (..),
    TrailingWhitespace (..),
    mapChildren,
    printsNothing,
  )
import Tilia.Settings (CommaStyle (..))
import Tilia.Span (Span, endOf)

----------------------------------------------------------------------------
-- Atoms

-- | A literal fragment of output.
--
-- The argument must not contain a line break; use 'hardBreak'. This is for
-- keywords, punctuation and names—anything whose spelling is fixed.
txt :: Text -> Doc
txt = DText

-- | A space. Repeated spaces collapse and a space before a line break is
-- dropped.
space :: Doc
space = DSpace

-- | A place the line may break. It becomes a line break if the enclosing
-- group is broken, and a space if it is flat. This is the workhorse: it
-- is what lets one printer serve both layouts.
breakOrSpace :: Doc
breakOrSpace = DBreak

-- | A place the line may break, leaving nothing behind if it does not. For
-- the positions where the two layouts differ by a break rather than by a
-- space, such as immediately inside a bracket.
breakOrNothing :: Doc
breakOrNothing = DSoftBreak

-- | A line break, whatever the enclosing group decided.
hardBreak :: Doc
hardBreak = DHardBreak

-- | An empty line.
blankLine :: Doc
blankLine = hardBreak <> hardBreak

-- | Lines of text reproduced rather than laid out: the lines of a block
-- comment, of a multi-line string literal, of a quasi-quotation.
verbatimLines ::
  -- | Where each line after the first begins.
  LineStart ->
  -- | What to do with the whitespace each line ends in.
  TrailingWhitespace ->
  -- | The lines.
  [Text] ->
  Doc
verbatimLines start trailing = sepBy (DVerbatimBreak start trailing) . fmap txt

-- | Text reproduced exactly, line breaks and all.
verbatim :: Text -> Doc
verbatim = verbatimLines AtMargin KeepWhitespace . T.splitOn "\n"

-- | An empty anchor so that comments can attach to it in situations when
-- nothing more substantial is present but the position is such that it
-- permits comments.
emptyAnchor :: Span -> Doc
emptyAnchor s = located s mempty

-- | Give the inside of a bracketed construct an anchor at its far end.
insideBrackets :: Maybe Span -> Doc -> Doc
insideBrackets here d = d <> foldMap (emptyAnchor . endOf) here

-- | Where a module's declarations begin, marked without printing anything.
declarationsStart :: Doc
declarationsStart = DDeclarationsStart

----------------------------------------------------------------------------
-- Layout

-- | Force flat layout.
flat :: Doc -> Doc
flat = DGroup Flat

-- | Force broken layout.
broken :: Doc -> Doc
broken = DGroup Broken

-- | Lay a document out flat where it fits within the column limit, and
-- broken where it does not.
frangible :: Doc -> Doc
frangible = DGroup Frangible

-- | Lay every 'Frangible' group of a document out flat, as it was written,
-- wherever it would run past the column limit.
infrangible ::
  -- | The document.
  Doc ->
  Doc
infrangible = \case
  DGroup Frangible d -> DGroup Flat (infrangible d)
  d -> mapChildren infrangible d

-- | Choose according to the layout the enclosing group settled on.
--
-- Reach for this only when the two layouts differ by more than where the
-- breaks fall.
variant ::
  -- | When flat.
  Doc ->
  -- | When broken.
  Doc ->
  Doc
variant = DVariant

-- | Record where the output is coming from in the input.
--
-- This has no effect on layout. It is provenance, kept so that later
-- passes—comment attachment above all—can ask which region of the input a
-- piece of the document corresponds to.
located :: Span -> Doc -> Doc
located = DLocated

-- | The same document, with no region in it for a comment to attach to.
unclaimed :: Doc -> Doc
unclaimed = \case
  DLocated _ d -> unclaimed d
  d -> mapChildren unclaimed d

-- | Keep the comments inside a region from floating out of it and
-- attaching to anything outside.
fence ::
  -- | The region, if there is one.
  Maybe Span ->
  -- | The document printed from it.
  Doc ->
  Doc
fence = maybe id DFence

-- | Alternatives the preprocessor chooses between.
cppChoice ::
  -- | The conditionals of the input the alternatives were printed from.
  [Conditional] ->
  -- | One alternative per directive, each directive as written after its
  -- hash.
  [(Text, Doc)] ->
  -- | The else clause.
  Doc ->
  Doc
cppChoice conditionals branches fallback
  | all (printsNothing . snd) branches && printsNothing fallback = DEmpty
  | otherwise =
      DCppChoice
        conditionals
        branches
        (if printsNothing fallback then DEmpty else fallback)

----------------------------------------------------------------------------
-- Attachment

-- | Whether a construct absorbs its own line break.
data Placement
  = -- | The preceding construct breaks and indents.
    Normal
  | -- | The construct is handed the rest of the line and breaks itself.
    Hanging
  deriving (Eq, Show)

-- | Join a body to whatever precedes it, according to its 'Placement'.
attach :: Placement -> Doc -> Doc
attach Hanging body = space <> body
attach Normal body = breakOrSpace <> indent body

----------------------------------------------------------------------------
-- Indentation

-- | Indent by the given number of steps, relative to the current level.
nest :: Int -> Doc -> Doc
nest = DNest . Steps

-- | Indent by one step.
indent :: Doc -> Doc
indent = DNest (Steps 1)

-- | Indent to the column the line has already reached, so that a broken
-- construct lines up under its own beginning rather than under the start of
-- the line.
align :: Doc -> Doc
align = DAlign

----------------------------------------------------------------------------
-- Combining

-- | Concatenate, separated by the given document.
sepBy :: Doc -> [Doc] -> Doc
sepBy s = mconcat . intersperse s

-- | The token that joins two parts of a construct: a space, the token, and
-- then the place the line may break.
joinedBy :: Text -> Doc
joinedBy t = space <> txt t <> breakOrSpace

----------------------------------------------------------------------------
-- Wrapping

-- | Where the closing bracket of a broken bracket pair goes.
data ClosingIndent
  = -- | Back out to the level the opening bracket is on.
    Outdented
  | -- | Kept one step in.
    Indented
  deriving (Eq, Show)

-- | Surround with a bracket pair that opens up when broken.
bracketWith ::
  -- | Where the closing bracket goes.
  ClosingIndent ->
  -- | Opening bracket.
  Text ->
  -- | Closing bracket.
  Text ->
  -- | Body.
  Doc ->
  Doc
bracketWith = commaBracketWith TrailingCommas

-- | A bracket pair around items separated by 'commaSep' in the given style,
-- lined up after the opening bracket, or with leading commas a step in where
-- the closing bracket is, since there no line may begin at the bracket's
-- column.
commaBracketWith ::
  -- | Where the commas go.
  CommaStyle ->
  -- | Where the closing bracket goes.
  ClosingIndent ->
  -- | Opening bracket.
  Text ->
  -- | Closing bracket.
  Text ->
  -- | The items, separated.
  Doc ->
  Doc
commaBracketWith commas closing open close body =
  align $
    txt open
      <> variant body (space <> linedUp body <> hardBreak)
      <> nest (closingSteps closing) (txt close)
  where
    linedUp = case (commas, closing) of
      (LeadingCommas, Indented) -> indent . hangingCommas commas
      _ -> align

-- | Surround with a bracket pair whose brackets are held off the body.
--
-- For the brackets that are more than one character wide—@(#@, @(|@—where
-- running the body up against them makes both harder to pick out, and where
-- an operator beginning with @#@ would lex as part of the bracket. Broken,
-- the body goes on its own indented lines.
spacedBracket ::
  -- | Where the closing bracket goes.
  ClosingIndent ->
  -- | Opening bracket.
  Text ->
  -- | Closing bracket.
  Text ->
  -- | Body.
  Doc ->
  Doc
spacedBracket closing open close body =
  align $
    txt open
      <> variant (space <> body <> space) (hardBreak <> indent body <> hardBreak)
      <> nest (closingSteps closing) (txt close)

closingSteps :: ClosingIndent -> Int
closingSteps = \case
  Outdented -> 0
  Indented -> 1

-- | @(@ and @)@.
parens ::
  -- | Body.
  Doc ->
  Doc
parens = bracketWith Outdented "(" ")"

-- | Surround with backticks.
backticks :: Doc -> Doc
backticks d = txt "`" <> d <> txt "`"

----------------------------------------------------------------------------
-- Punctuation

-- | @,@.
comma :: Doc
comma = txt ","

-- | A comma between two items, with the place the line may break: after
-- it, or, with 'LeadingCommas', before it, at the indentation in force.
separatingComma ::
  -- | Where the commas go.
  CommaStyle ->
  Doc
separatingComma = \case
  TrailingCommas -> comma <> breakOrSpace
  LeadingCommas -> breakOrNothing <> DNest (Columns (-2)) (txt ", ")

-- | Move items separated by 'commaSep' right by the width of a leading
-- comma, so that the commas, not the items, come where the items would
-- with trailing commas.
hangingCommas ::
  -- | Where the commas go.
  CommaStyle ->
  -- | The items, separated.
  Doc ->
  Doc
hangingCommas = \case
  TrailingCommas -> id
  LeadingCommas -> DNest (Columns 2)

-- | Separate by 'separatingComma', with leading commas under the first
-- item rather than to the left of it.
commaSepUnderFirst ::
  -- | Where the commas go.
  CommaStyle ->
  -- | The items.
  [Doc] ->
  Doc
commaSepUnderFirst commas = \case
  [] -> mempty
  x : xs -> x <> hangingCommas commas (foldMap (separatingComma commas <>) xs)

-- | Separate by 'separatingComma'.
commaSep ::
  -- | Where the commas go.
  CommaStyle ->
  -- | The items.
  [Doc] ->
  Doc
commaSep = sepBy . separatingComma

-- | Items after a lead-in such as a bar, separated by commas and lined up
-- under the first.
commaSepAfter ::
  -- | Where the commas go.
  CommaStyle ->
  -- | The lead-in.
  Text ->
  -- | The items.
  [Doc] ->
  Doc
commaSepAfter commas leadIn items =
  txt leadIn <> space <> align (commaSep commas items)

----------------------------------------------------------------------------
-- Conditionals

-- | The document if the condition holds, nothing otherwise.
includeWhen :: Bool -> Doc -> Doc
includeWhen b d = if b then d else mempty

-- | The document unless the condition holds.
includeUnless :: Bool -> Doc -> Doc
includeUnless b = includeWhen (not b)
