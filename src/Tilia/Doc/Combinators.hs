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
    itemGap,
    declarationsStart,

    -- * Layout
    Layout (..),
    flat,
    broken,
    frangible,
    infrangible,
    variant,
    trailingOrLeading,
    endingItsLine,
    located,
    unclaimed,
    fence,
    cppChoice,

    -- * Attachment
    Placement (..),
    attach,

    -- * Indentation
    ref,
    refAt,
    indent,
    (>+>),
    ptr,
    align,

    -- * Combining
    sepBy,
    joinedBy,

    -- * Wrapping
    parens,
    openingAt,
    closingAt,
    bracketWith,
    commaBracketWith,
    spacedBracket,
    backticks,

    -- * Punctuation
    CommaStyle (..),
    comma,
    trailing,
    leading,
    leadingBar,
    leadingDot,
    leadingOperator,
    separatingComma,
    separatingSemicolon,
    commaSep,
    commaWidth,
    commaSepUnderFirst,
    commaSepAfter,

    -- * Inclusion
    includeWhen,
    includeUnless,
  )
where

import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as T

import Tilia.Doc
  ( BracketSide (..),
    ChosenBy (..),
    Conditional (..),
    Doc (..),
    Layout (..),
    LineStart (..),
    SeparatorSide (..),
    Shape (..),
    TrailingWhitespace (..),
    mapChildren,
    printsNothing,
    regionOf,
  )
import Tilia.Settings (CommaStyle (..))
import Tilia.Span (Span, endOf, startOf)

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
verbatimLines start whitespace =
  sepBy (DVerbatimBreak start whitespace) . fmap txt

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

-- | The break that leaves an empty line between two items of a run, so
-- that the comments written right under the first can go with it.
itemGap ::
  -- | The break.
  Doc ->
  Doc
itemGap = DGap

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
variant = DVariant ByLayout

-- | Parts with their separators trailing the parts before them, or leading
-- the parts after them where a comment would leave one alone at the start
-- of a line.
trailingOrLeading ::
  -- | With the separators trailing.
  Doc ->
  -- | With the separators leading.
  Doc ->
  Doc
trailingOrLeading = DVariant BySeparators

-- | A comment that ends its line, as a Haddock written as a @--@ line does.
endingItsLine ::
  -- | Where it was written.
  Span ->
  -- | The comment.
  Doc ->
  Doc
endingItsLine = DComment EndsTheLine

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
  = -- | The construct begins a line of its own where the layout breaks.
    Normal
  | -- | The construct is handed the rest of the line and breaks itself.
    Hanging
  deriving (Eq, Show)

-- | A body after the part it is placed relative to, joined to it according
-- to its 'Placement'.
attach ::
  -- | How the body is joined.
  Placement ->
  -- | The body.
  Doc ->
  Doc
attach Hanging body = space <> body
attach Normal body = breakOrSpace <> body

----------------------------------------------------------------------------
-- Indentation

-- | A referent that 'indent' measures from, identified by the regions it
-- holds.
ref :: Doc -> Doc
ref = \case
  a@DReferent{} -> a
  a -> DReferent (referentOf a) a

-- | 'ref' for a part holding no region, such as a keyword the comments
-- around it do not attach to, identified by where it was written.
refAt :: Maybe Span -> Doc -> Doc
refAt = DReferent

-- | A referent that prints nothing but shares an identity with a part that
-- is printed elsewhere.
ptr :: Doc -> Doc
ptr a = DReferent (referentOf a) mempty

-- | What a part is named by as a 'ref': the regions it holds.
referentOf :: Doc -> Maybe Span
referentOf = \case
  DReferent k _ -> k
  a -> regionOf a

-- | Print the first part, then the second indented by this many steps in
-- relation to the referent in the first part.
indent ::
  -- | The steps.
  Int ->
  -- | The first part.
  Doc ->
  -- | The second part.
  Doc ->
  Doc
indent n a = \case
  DEmpty
    | printsNothing a -> DEmpty
    | otherwise -> ref a
  b -> DIndented n (ref a) b

infixr 5 >+>

-- | 'indent' by one step.
(>+>) ::
  -- | The first part.
  Doc ->
  -- | The second part.
  Doc ->
  Doc
(>+>) = indent 1

-- | Indent to the column the line has already reached, so that a broken
-- construct lines up under its own beginning rather than under the start of
-- the line.
align :: Doc -> Doc
align = DAligned 0

----------------------------------------------------------------------------
-- Combining

-- | Concatenate, separated by the given document.
sepBy :: Doc -> [Doc] -> Doc
sepBy s = mconcat . intersperse s

-- | The token that joins two parts of a construct: a space, the token, and
-- then the place the line may break.
joinedBy :: Text -> Doc
joinedBy t = trailing (txt t) <> breakOrSpace

----------------------------------------------------------------------------
-- Wrapping

-- | Surround with a bracket pair that opens up when broken.
bracketWith ::
  -- | How many steps in from the opening bracket the closing one goes.
  Int ->
  -- | Where the brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | Opening bracket.
  Text ->
  -- | Closing bracket.
  Text ->
  -- | Body.
  Doc ->
  Doc
bracketWith = commaBracketWith TrailingCommas

-- | A bracket pair around items separated by 'commaSep' in the given style,
-- lined up after the opening bracket, or with leading commas under the first
-- item where the closing bracket goes a step in, since there no line may
-- begin at the bracket's column.
commaBracketWith ::
  -- | Where the commas go.
  CommaStyle ->
  -- | How many steps in from the opening bracket the closing one goes.
  Int ->
  -- | Where the brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | Opening bracket.
  Text ->
  -- | Closing bracket.
  Text ->
  -- | The items, separated.
  Doc ->
  Doc
commaBracketWith commas closing (o, c) open close body =
  align $
    opener
      <> variant body (space <> linedUp body <> hardBreak)
      <> indent closing (ptr opener) (closingAt c close)
  where
    opener = openerOf body (openingAt o open)
    linedUp = DAligned $ case commas of
      LeadingCommas | closing > 0 -> commaWidth commas
      _ -> 0

-- | Surround with a bracket pair whose brackets are held off the body.
--
-- For the brackets that are more than one character wide—@(#@, @(|@—where
-- running the body up against them makes both harder to pick out, and where
-- an operator beginning with @#@ would lex as part of the bracket. Broken,
-- the body goes on its own indented lines.
spacedBracket ::
  -- | How many columns past a step a broken body goes, for the commas
  -- leading its lines.
  Int ->
  -- | How many steps in from the opening bracket the closing one goes.
  Int ->
  -- | Where the brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | Opening bracket.
  Text ->
  -- | Closing bracket.
  Text ->
  -- | Body.
  Doc ->
  Doc
spacedBracket columns closing (o, c) open close body =
  align $
    opener
      <> variant
        (space <> body <> space)
        ( hardBreak
            <> indent 1 (ptr opener) (DAligned columns body)
            <> hardBreak
        )
      <> indent closing (ptr opener) (closingAt c close)
  where
    opener = openerOf body (openingAt o open)

-- | An opening bracket, a 'ref' named by the start of what it holds.
openerOf ::
  -- | What it holds.
  Doc ->
  -- | The bracket.
  Doc ->
  Doc
openerOf body = refAt (startOf <$> regionOf body)

-- | An opening bracket, marked with where it was written.
openingAt ::
  -- | Where it was written.
  Maybe Span ->
  -- | The bracket.
  Text ->
  Doc
openingAt s = DBracket OpeningBracket s . txt

-- | A closing bracket, marked with where it was written.
closingAt ::
  -- | Where it was written.
  Maybe Span ->
  -- | The bracket.
  Text ->
  Doc
closingAt s = DBracket ClosingBracket s . txt

-- | @(@ and @)@.
parens ::
  -- | Where they were written.
  (Maybe Span, Maybe Span) ->
  -- | Body.
  Doc ->
  Doc
parens at = bracketWith 0 at "(" ")"

-- | Surround with backticks.
backticks :: Doc -> Doc
backticks d = txt "`" <> d <> txt "`"

----------------------------------------------------------------------------
-- Punctuation

-- | @,@.
comma :: Doc
comma = DSeparator TrailingSeparator (txt "," <> space) (txt ",")

-- | A leading separator which is in the common case followed by a space,
-- but if it is rearranged by the merge the space is put in front of it.
leading :: Doc -> Doc
leading d = DSeparator LeadingSeparator (d <> space) (space <> d)

-- | A trailing separator which is in the common case preceded by a space,
-- but if it is rearranged by the merge the space is put after it.
trailing ::
  -- | The token.
  Doc ->
  Doc
trailing d = DSeparator TrailingSeparator (d <> space) (space <> d)

-- | An operator leading its operand, which trailing the part before it is
-- written after a space.
leadingOperator ::
  -- | The operator.
  Doc ->
  Doc
leadingOperator d = DSeparator LeadingSeparator d (space <> d)

-- | @|@ leading the part after it.
leadingBar :: Doc
leadingBar = leading (txt "|")

-- | The dot of a @forall@ leading the part after it, a column right of the
-- indentation where it begins a line.
leadingDot :: Doc
leadingDot =
  DSeparator LeadingSeparator (DAligned 1 (txt ".") <> space) (txt ".")

-- | A comma between two items, with the place the line may break: after
-- it, or, with 'LeadingCommas', before it, hung left of the indentation by
-- its width.
separatingComma ::
  -- | Where the commas go.
  CommaStyle ->
  Doc
separatingComma = \case
  TrailingCommas -> comma <> breakOrSpace
  LeadingCommas -> breakOrNothing <> hung ","

-- | A semicolon leading the item after it, with the place the line may
-- break before it, hung left of the indentation by its width.
separatingSemicolon :: Doc
separatingSemicolon = breakOrNothing <> hung ";"

-- | A separator and a space leading the item after it, hung left of the
-- column they begin at by their width.
hung ::
  -- | The separator.
  Text ->
  Doc
hung t =
  DSeparator
    LeadingSeparator
    (DAligned (negate (T.length t + 1)) (txt t <> space))
    (txt t)

-- | How many columns a comma takes where it leads the item after it.
commaWidth ::
  -- | Where the commas go.
  CommaStyle ->
  Int
commaWidth = \case
  TrailingCommas -> 0
  LeadingCommas -> 2

-- | Separate by 'separatingComma', with the leading commas of the items
-- after the first so many steps in from it and those items past them.
commaSepUnderFirst ::
  -- | Where the commas go.
  CommaStyle ->
  -- | How many steps in from the first item the others go.
  Int ->
  -- | The items.
  [Doc] ->
  Doc
commaSepUnderFirst commas steps = \case
  [] -> mempty
  [x] -> x
  x : xs -> ref x <> indent steps (ptr x) rest
    where
      rest = case commas of
        TrailingCommas -> separatingComma commas <> commaSep commas xs
        LeadingCommas ->
          breakOrNothing
            <> DAligned (commaWidth commas) (hung "," <> commaSep commas xs)

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
  leading (txt leadIn) <> align (commaSep commas items)

----------------------------------------------------------------------------
-- Inclusion

-- | The document if the condition holds, nothing otherwise.
includeWhen :: Bool -> Doc -> Doc
includeWhen b d = if b then d else mempty

-- | The document unless the condition holds.
includeUnless :: Bool -> Doc -> Doc
includeUnless b = includeWhen (not b)
