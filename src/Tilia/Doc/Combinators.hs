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
    Resume (..),
    verbatimBreak,
    verbatim,
    emptyAnchor,

    -- * Layout
    Layout (..),
    group,
    flat,
    broken,
    variant,
    located,
    fence,
    cppChoice,

    -- * Attachment
    Placement (..),
    attach,
    hangingIfSingleLine,

    -- * Indentation
    nest,
    indent,
    align,

    -- * Combining
    hsep,
    vsep,
    sepBy,
    joinedBy,
    punctuate,

    -- * Wrapping
    ClosingIndent (..),
    bracket,
    parens,
    parensWith,
    brackets,
    bracketsWith,
    braces,
    bananaWith,
    unboxed,
    unboxedWith,
    backticks,

    -- * Punctuation
    comma,
    commaSep,
    semi,

    -- * Conditionals
    includeWhen,
    includeUnless,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Span (Span, isSingleLine)
import Tilia.Doc.Internal
  ( Doc (..),
    Layout (..),
    Resume (..),
    groupLayout,
  )

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
-- 'group' is broken, and a space if it is flat. This is the workhorse: it
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

-- | A line break between two lines of text that is being reproduced.
--
-- Only for text that is being reproduced rather than laid out: the lines of
-- a block comment, of a multi-line string literal, of a quasi-quotation.
-- Unlike every other break this one collapses nothing, because an empty line
-- among those is the author's and not spacing.
verbatimBreak :: Resume -> Doc
verbatimBreak = DVerbatimBreak

-- | Text reproduced exactly, line breaks and all.
verbatim :: Text -> Doc
verbatim = sepBy (verbatimBreak AtMargin) . map txt . T.splitOn "\n"

-- | An anchor for a construct that contains nothing.
emptyAnchor :: Span -> Doc
emptyAnchor s = located s mempty

----------------------------------------------------------------------------
-- Layout

-- | Lay the document out as the input had it: flat if the construct was
-- written on one line, broken if it was spread across several.
group :: Span -> Doc -> Doc
group s = DGroup (groupLayout (Just s))

-- | Force flat layout.
flat :: Doc -> Doc
flat = DGroup Flat

-- | Force broken layout.
broken :: Doc -> Doc
broken = DGroup Broken

-- | Choose according to the layout the enclosing 'group' settled on.
--
-- Reach for this only when the two layouts differ by more than where the
-- breaks fall; when they differ only in that, 'breakOrSpace' and
-- 'breakOrNothing' already say so and read better.
variant ::
  -- | When flat
  Doc ->
  -- | When broken
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

-- | Fence prevents comments inside from floating out and attaching to
-- elements they are not supposed to attach to.
fence :: Span -> Doc -> Doc
fence = DFence

-- | Alternatives the preprocessor chooses between.
cppChoice ::
  -- | One alternative per directive, each directive as written after its
  -- hash
  [(Text, Doc)] ->
  -- | What holds when none of them applies
  Doc ->
  Doc
cppChoice = DCppChoice

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

-- | 'Hanging' if the span was a single line in the input, 'Normal'
-- otherwise.
--
-- A handful of constructs hang only when what comes before their own break
-- was written on one line—a lambda whose parameters ran on, for instance,
-- would leave the body indented under nothing legible. Those constructs
-- consult the input, exactly as 'group' does, and this is the shared
-- spelling of that question so that it reads as policy rather than as a
-- special case repeated in each classifier.
hangingIfSingleLine :: Span -> Placement
hangingIfSingleLine s = if isSingleLine s then Hanging else Normal

----------------------------------------------------------------------------
-- Indentation

-- | Indent by the given number of steps, relative to the current level.
nest :: Int -> Doc -> Doc
nest = DNest

-- | Indent by one step.
indent :: Doc -> Doc
indent = DNest 1

-- | Indent to the column the line has already reached, so that a broken
-- construct lines up under its own beginning rather than under the start of
-- the line.
align :: Doc -> Doc
align = DAlign

----------------------------------------------------------------------------
-- Combining

-- | Concatenate, separated by 'space'.
hsep :: [Doc] -> Doc
hsep = sepBy space

-- | Concatenate, separated by 'hardBreak'.
vsep :: [Doc] -> Doc
vsep = sepBy hardBreak

-- | Concatenate, separated by the given document.
sepBy :: Doc -> [Doc] -> Doc
sepBy s = mconcat . intersperse s

-- | The token that joins two parts of a construct: a space, the token, and
-- then the place the line may break.
joinedBy :: Text -> Doc
joinedBy t = space <> txt t <> breakOrSpace

-- | Append the separator to every element but the last.
--
-- For the cases where the separator has to travel with the element rather
-- than sit between elements, such as a trailing comma that must stay on the
-- line above a break.
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [x] = [x]
punctuate s (x : xs) = (x <> s) : punctuate s xs

----------------------------------------------------------------------------
-- Wrapping

-- | Surround with the given opening and closing documents, adding nothing
-- of its own.
enclose ::
  -- | Opening bracket
  Doc ->
  -- | Closing bracket
  Doc ->
  -- | Body
  Doc ->
  Doc
enclose open close body = open <> body <> close

-- | Where the closing bracket of a broken bracket pair goes.
data ClosingIndent
  = -- | Back out to the level the opening bracket is on.
    Outdented
  | -- | Kept one step in.
    Indented
  deriving (Eq, Show)

-- | Surround with a bracket pair that opens up when broken.
--
-- Flat, this is @open body close@ with nothing added. Broken, the opening
-- bracket keeps the first line of the body company and the rest of the body
-- lines up under it, with the closing bracket alone on the last line:
--
-- > ( first,
-- >   second
-- > )
bracket ::
  -- | Opening bracket
  Text ->
  -- | Closing bracket
  Text ->
  -- | Body
  Doc ->
  Doc
bracket = bracketWith Outdented

-- | 'bracket', with a say in where the closing bracket goes.
bracketWith ::
  -- | Where the closing bracket goes
  ClosingIndent ->
  -- | Opening bracket
  Text ->
  -- | Closing bracket
  Text ->
  -- | Body
  Doc ->
  Doc
bracketWith closing open close body =
  -- The pair is aligned as a whole so that the closing bracket comes back
  -- out to the column the opening one is on, wherever on its line that was.
  align $
    txt open
      <> variant body (space <> align body <> hardBreak)
      <> nest (closingSteps closing) (txt close)

-- | Surround with a bracket pair whose brackets are held off the body.
--
-- For the brackets that are more than one character wide—@(#@, @(|@—where
-- running the body up against them makes both harder to pick out, and where
-- an operator beginning with @#@ would lex as part of the bracket. Broken,
-- the body goes on its own indented lines.
spacedBracket ::
  -- | Where the closing bracket goes
  ClosingIndent ->
  -- | Opening bracket
  Text ->
  -- | Closing bracket
  Text ->
  -- | Body
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
parens :: Doc -> Doc
parens = bracket "(" ")"

-- | @(@ and @)@, with a say in where the closing bracket goes.
parensWith ::
  -- | Where the closing parenthesis goes
  ClosingIndent ->
  -- | Body
  Doc ->
  Doc
parensWith closing = bracketWith closing "(" ")"

-- | @[@ and @]@.
brackets :: Doc -> Doc
brackets = bracket "[" "]"

-- | @[@ and @]@, with a say in where the closing bracket goes.
bracketsWith ::
  -- | Where the closing bracket goes
  ClosingIndent ->
  -- | Body
  Doc ->
  Doc
bracketsWith closing = bracketWith closing "[" "]"

-- | @{@ and @}@.
braces :: Doc -> Doc
braces = bracket "{" "}"

-- | @(|@ and @|)@, from arrow notation, with a say in where the closing
-- bracket goes.
bananaWith ::
  -- | Where the closing banana goes
  ClosingIndent ->
  -- | Body
  Doc ->
  Doc
bananaWith closing = spacedBracket closing "(|" "|)"

-- | @(#@ and @#)@, for unboxed tuples and sums.
unboxed :: Doc -> Doc
unboxed = unboxedWith Outdented

-- | @(#@ and @#)@, with a say in where the closing bracket goes.
unboxedWith ::
  -- | Where the closing bracket goes
  ClosingIndent ->
  -- | Body
  Doc ->
  Doc
unboxedWith closing = spacedBracket closing "(#" "#)"

-- | Surround with backticks.
backticks :: Doc -> Doc
backticks = enclose (txt "`") (txt "`")

----------------------------------------------------------------------------
-- Punctuation

-- | @,@.
comma :: Doc
comma = txt ","

-- | @;@.
semi :: Doc
semi = txt ";"

-- | Separate by a comma and a 'breakOrSpace', so that a broken list puts each
-- element on its own line with the comma left behind on the one above.
commaSep :: [Doc] -> Doc
commaSep = sepBy (comma <> breakOrSpace)

----------------------------------------------------------------------------
-- Conditionals

-- | The document if the condition holds, nothing otherwise.
includeWhen :: Bool -> Doc -> Doc
includeWhen b d = if b then d else mempty

-- | The document unless the condition holds.
includeUnless :: Bool -> Doc -> Doc
includeUnless b = includeWhen (not b)
