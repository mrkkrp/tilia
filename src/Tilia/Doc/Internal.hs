{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The document representation and the engine that turns it into text.
module Tilia.Doc.Internal
  ( -- * Documents
    Doc (..),
    Spill (..),
    Shape (..),
    LineStart (..),
    TrailingWhitespace (..),
    SeparatorSide (..),
    BracketSide (..),
    Layout (..),
    ChosenBy (..),
    Conditional (..),
    conditionalRange,
    chosen,
    printsNothing,
    namesOnly,
    referentIn,
    Wrapper (..),
    unwrap,
    wrap,
    layoutInside,

    -- * Rendering
    printDoc,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (asum)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List (unsnoc)
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Tilia.Settings (Settings (..), indentationColumns)
import Tilia.Source.Lines (endsInBackslash)
import Tilia.Span (Span)
import Tilia.Utils (opensLineComment)

----------------------------------------------------------------------------
-- Documents

-- | A description of what to print.
data Doc
  = -- | Print nothing.
    DEmpty
  | -- | A literal fragment. Must not contain a line break: the engine
    -- tracks columns by counting characters, and an embedded newline would
    -- make that count wrong. Use 'DHardBreak'.
    DText !Text
  | -- | A space. Repeats collapse, and one at the end of a line is dropped,
    -- so printing code may emit them freely rather than working out whether
    -- one is already there.
    DSpace
  | -- | A space when the enclosing group is flat, a line break when it is
    -- broken.
    DBreak
  | -- | Nothing when the enclosing group is flat, a line break when it is
    -- broken.
    DSoftBreak
  | -- | A line break regardless of the enclosing group.
    DHardBreak
  | -- | Close the line, and let a break that immediately follows know that
    -- it has nothing left to do.
    DCloseLine
  | -- | A line break between two lines of text that is being reproduced
    -- rather than laid out.
    DVerbatimBreak !LineStart !TrailingWhitespace
  | -- | Concatenation. See the 'Semigroup' instance.
    DCat !Doc !Doc
  | -- | A referent identifies an element which provides the reference
    -- indentation level for relative indentation encoded by 'DIndented'.
    DReferent !(Maybe Span) !Doc
  | -- | Print the left 'Doc', then the right one indented by so many steps
    -- relative to the first referent found in the left 'Doc'. Steps entered
    -- on one line with no line begun between them count once: the same
    -- principle as with consecutive newlines and spaces.
    DIndented !Int !Doc !Doc
  | -- | Print the 'Doc' with its lines so many columns right of the column
    -- it begins at. This interrupts collapsing of consecutive indentation
    -- steps.
    DAligned !Int !Doc
  | -- | Start the enclosed document's lines at the margin if they come out
    -- right above a preprocessor directive.
    DCppMarginNote !Doc
  | -- | Lay the enclosed document out flat or broken.
    DGroup !Layout !Doc
  | -- | Two layouts of one piece of code, and what chooses between them.
    --
    -- Both layouts are lazy, and that is not an oversight. The engine walks
    -- one of them and never looks at the other, so the branch not taken
    -- should cost nothing. Were they strict, building a variant would build
    -- both layouts of everything inside it; a construct nested @n@ deep
    -- would be built @2^n@ times.
    DVariant !ChosenBy Doc Doc
  | -- | Record that the enclosed document was produced from the given
    -- region of the input.
    --
    -- This carries no layout meaning and the engine ignores it. Keeping
    -- provenance separate from grouping is deliberate: the two coincide
    -- often, but a construct can need one without the other, and fusing
    -- them is what forces a printer to grow an escape hatch for each case
    -- where they come apart.
    DLocated !Span !Doc
  | -- | Fence prevents comments inside from floating out and attaching to
    -- elements they are not supposed to attach to.
    DFence !Span !Doc
  | -- | A separator between two parts, such as a comma: the side of the
    -- part it goes with it is written on, how it is written leading the
    -- part after it, and how it is written trailing the part before it.
    -- The engine prints the form for the side; the other form is for the
    -- merge, which may move it.
    DSeparator !SeparatorSide !Doc !Doc
  | -- | A bracket, which of its pair it is, and where it was written.
    DBracket !BracketSide !(Maybe Span) !Doc
  | -- | A comment, put among the lines around it as its shape says, and
    -- where it was written.
    DComment !Shape !Span !Doc
  | -- | Alternatives the preprocessor chooses between, the condition it
    -- chooses on, and the conditionals of the input they were printed from.
    DCppChoice ![Conditional] ![(Text, Doc)] !Doc
  | -- | A preprocessor line that is not a conditional, reproduced, and the
    -- region of the input it was written in. The span is included so that
    -- two directives can be told apart.
    DCppDirective !Span !Text
  | -- | Print nothing, marking where a module's declarations begin.
    DDeclarationsStart
  deriving (Eq, Show)

-- | Where text held back for the end of a line goes when something was held
-- back for it first.
data Spill
  = -- | On a line of its own, at the indentation of the line it was held
    -- back for.
    SpillAtIndentation
  | -- | On a line of its own, lined up with what was held back before it.
    SpillUnderPrevious
  | -- | On a line of its own at the margin, leaving what is held back after
    -- it to line up as if it were not there.
    SpillAtMargin
  deriving (Eq, Show)

-- | How a comment is printed among the lines of the code around it.
data Shape
  = -- | Spliced into its line, with code able to follow it there.
    InPlace
  | -- | After what comes before it on its line, closing the line.
    EndsTheLine
  | -- | Held back to the end of whatever line of output it lands on,
    -- however much of that line is still to be written.
    HeldBack !Spill
  | -- | On lines of its own, under an empty line where told to, but on the
    -- line of an opener it follows, with nothing held back for that line.
    OnItsOwnLines !Bool
  deriving (Eq, Show)

-- | Where the line after a 'DVerbatimBreak' begins.
data LineStart
  = -- | At the indentation in force, as any other break would.
    AtIndent
  | -- | At column zero, whatever the indentation.
    AtMargin
  deriving (Eq, Show)

-- | Which side of the part it goes with a 'DSeparator' is written on.
data SeparatorSide
  = -- | Before it: the separator leads the part after it.
    LeadingSeparator
  | -- | After it: the separator trails the part before it.
    TrailingSeparator
  deriving (Eq, Show)

-- | Which of its pair a 'DBracket' is.
data BracketSide
  = -- | The one that opens what the pair holds.
    OpeningBracket
  | -- | The one that closes it.
    ClosingBracket
  deriving (Eq, Show)

-- | What to do with the whitespace a finished line ends in.
data TrailingWhitespace
  = -- | Trim it.
    TrimWhitespace
  | -- | Keep it.
    KeepWhitespace
  deriving (Eq, Show)

-- | Whether a group is laid out on one line or across several.
data Layout
  = -- | On one line, every break printed as a space or as nothing.
    Flat
  | -- | Across lines, every break printed as a line break.
    Broken
  | -- | Flat where it fits within the column limit, broken where it does
    -- not.
    Frangible
  deriving (Eq, Show)

-- | What chooses between the two layouts of a 'DVariant'.
data ChosenBy
  = -- | The enclosing group: the first when it is flat, the second when it
    -- is broken.
    ByLayout
  | -- | The separators of the first, which trail the parts before them: the
    -- first, unless a comment ends the line right before one of them, and
    -- the second, where they lead, if it does.
    BySeparators
  deriving (Eq, Show)

-- | A conditional as its author wrote it.
data Conditional = Conditional
  { -- | The lines of its directives, the @#if@ first and the @#endif@ last.
    conditionalLines :: [Int],
    -- | What follows the hash of its @#if@ and of each @#elif@.
    conditionalGuards :: [Text],
    -- | What follows the keyword of its @#else@.
    conditionalElse :: Text,
    -- | What follows the keyword of its @#endif@.
    conditionalEndif :: Text
  }
  deriving (Eq, Ord, Show)

-- | The lines of a conditional's @#if@ and @#endif@.
conditionalRange :: Conditional -> Maybe (Int, Int)
conditionalRange c =
  (,) <$> listToMaybe ls <*> fmap snd (unsnoc ls)
  where
    ls = conditionalLines c

-- | Does this document put nothing at all on the page?
printsNothing ::
  -- | The document.
  Doc ->
  Bool
printsNothing = \case
  DEmpty -> True
  DDeclarationsStart -> True
  DCat a b -> printsNothing a && printsNothing b
  DIndented _ a b -> printsNothing a && printsNothing b
  DVariant _ a b -> printsNothing a && printsNothing b
  d -> maybe False (printsNothing . snd) (unwrap d)

-- | Is this document nothing but pointers to parts printed elsewhere?
namesOnly ::
  -- | The document.
  Doc ->
  Bool
namesOnly = \case
  DEmpty -> True
  DReferent _ DEmpty -> True
  DCat a b -> namesOnly a && namesOnly b
  _ -> False

-- | The name of the first referent a document holds, if it holds one.
referentIn ::
  -- | The document.
  Doc ->
  Maybe (Maybe Span)
referentIn = \case
  DReferent k _ -> Just k
  DCat a b -> referentIn a <|> referentIn b
  DIndented _ a b -> referentIn a <|> referentIn b
  DVariant _ a b -> referentIn a <|> referentIn b
  DCppChoice _ bs e -> asum (fmap (referentIn . snd) bs) <|> referentIn e
  d -> referentIn . snd =<< unwrap d

-- | The names of the referents a document holds side by side.
referentsIn ::
  -- | The document.
  Doc ->
  [Maybe Span]
referentsIn = \case
  DReferent k _ -> [k]
  DCat a b -> referentsIn a <> referentsIn b
  _ -> []

-- | A node holding exactly one document, less the document.
data Wrapper
  = WLocated !Span
  | WFence !Span
  | -- | A 'DSeparator' less the form for its side, holding the other.
    WSeparator !SeparatorSide !Doc
  | WBracket !BracketSide !(Maybe Span)
  | WComment !Shape !Span
  | WReferent !(Maybe Span)
  | WIndented !Int !Doc
  | WAligned !Int
  | WCppMarginNote
  | WGroup !Layout
  deriving (Eq, Show)

-- | A document as the wrapper it is and the document it holds.
unwrap ::
  -- | The document.
  Doc ->
  Maybe (Wrapper, Doc)
unwrap = \case
  DLocated s d -> Just (WLocated s, d)
  DFence s d -> Just (WFence s, d)
  DSeparator LeadingSeparator l t -> Just (WSeparator LeadingSeparator t, l)
  DSeparator TrailingSeparator l t -> Just (WSeparator TrailingSeparator l, t)
  DBracket b s d -> Just (WBracket b s, d)
  DComment s c d -> Just (WComment s c, d)
  DReferent k d -> Just (WReferent k, d)
  DIndented n a d | namesOnly a -> Just (WIndented n a, d)
  DAligned n d -> Just (WAligned n, d)
  DCppMarginNote d -> Just (WCppMarginNote, d)
  DGroup l d -> Just (WGroup l, d)
  _ -> Nothing
{-# INLINE unwrap #-}

-- | Put a document in a wrapper.
wrap ::
  -- | The wrapper.
  Wrapper ->
  -- | The document to put in it.
  Doc ->
  Doc
wrap = \case
  WLocated s -> DLocated s
  WFence s -> DFence s
  WSeparator LeadingSeparator t -> \l -> DSeparator LeadingSeparator l t
  WSeparator TrailingSeparator l -> DSeparator TrailingSeparator l
  WBracket b s -> DBracket b s
  WComment s c -> DComment s c
  WReferent k -> DReferent k
  WIndented n a -> DIndented n a
  WAligned n -> DAligned n
  WCppMarginNote -> DCppMarginNote
  WGroup l -> DGroup l

-- | The layout inside a wrapper laid out like this.
layoutInside ::
  -- | The layout of the group the wrapper is printed in.
  Layout ->
  -- | The wrapper.
  Wrapper ->
  Layout
layoutInside layout = \case
  WGroup l -> l
  _ -> layout

instance Semigroup Doc where
  DEmpty <> b = b
  a <> DEmpty = a
  a <> b = DCat a b

instance Monoid Doc where
  mempty = DEmpty

----------------------------------------------------------------------------
-- Rendering

-- | Columns per indentation step.
step ::
  -- | What the document is printed with.
  Settings ->
  Int
step = indentationColumns . settingsIndentation

-- | The indentation so many steps in from another, where a step wider than
-- two columns is taken from the nearest multiple of itself at or left of
-- it, as Fourmolu does.
nestedBy ::
  -- | Columns per indentation step.
  Int ->
  -- | The steps.
  Int ->
  -- | The indentation to step in from.
  Int ->
  Int
nestedBy columns steps indent
  | columns > 2, steps > 0 = indent `div` columns * columns + steps * columns
  | otherwise = indent + steps * columns

-- | What the engine carries while walking a document.
data Env = Env
  { -- | The column a new line starts at.
    envIndent :: !Int,
    -- | The indentation without the steps of the innermost 'DIndented'.
    envBase :: !Int,
    -- | The line, counted by 'outBegun', the innermost 'DIndented' was
    -- entered on, unless a 'DAligned' was entered inside it.
    envWithin :: !Int,
    -- | The layout of the innermost group as it settled, 'Frangible' where
    -- a frangible group settled flat.
    envLayout :: !Layout,
    -- | Whether what is written now is a note, which goes to the margin
    -- right above a preprocessor directive.
    envNote :: !Bool,
    -- | Whether a group laid out flat holds what is written now, with no
    -- broken group in between, which keeps every 'Frangible' group in it
    -- flat.
    envHeldFlat :: !Bool,
    -- | What the document is printed with.
    envSettings :: !Settings
  }

-- | Output built so far.
--
-- Lines are finished one at a time and never revisited while the document
-- is walked, but for the empty lines a directive drops right above itself.
-- Whatever about a line depends on the lines around it is settled only
-- once they are all there, in 'finish'.
data Out = Out
  { -- | Completed lines, most recent first.
    outLines :: [Line],
    -- | The line being built, but for its body, which is put together from
    -- 'outCurrent' and 'outHeldBack' once it is finished.
    outLine :: !Line,
    -- | What has been written to the line being built after its
    -- indentation, most recent first.
    outCurrent :: [Text],
    -- | Column the current line has reached.
    outColumn :: !Int,
    -- | Fragments held back until the line ends, in the order they were
    -- given. The first goes at the end of the line; any after it get lines
    -- of their own under it.
    outHeldBack :: ![(Spill, Text)],
    -- | The first remark held back for the line, and the column the line
    -- had reached when it was.
    outRemark :: !(Maybe (Span, Int)),
    -- | The remarks held back for a line that another comment was written
    -- on after code, which end their lines where they stand when the
    -- document is printed again.
    outCrowded :: ![Span],
    -- | What the line being built ends with, so far as what is written
    -- next depends on it.
    outEnding :: !Ending,
    -- | The base of each referent written so far, by its region: its
    -- indentation without the steps entered on the line it begins on,
    -- which steps from it count from.
    outReferents :: !(HashMap (Maybe Span) Int),
    -- | The referents being written that no text was written in yet, each
    -- with the line, counted by 'outBegun', it was entered on, if that had
    -- begun, and the indentation it was entered at.
    outUnwritten :: [(Maybe Span, Int, Int)],
    -- | The referents left with no text written in them, which a later part
    -- of theirs places again.
    outBlank :: [Maybe Span],
    -- | How many lines were begun.
    outBegun :: !Int
  }

-- | What the line being built ends with, so far as what is written next
-- depends on it.
data Ending
  = -- | Whatever was written to it last, or nothing yet.
    Written
  | -- | Nothing, since something that knew it was ending the line before
    -- closed it: a break arriving now would add an empty line rather than
    -- end anything.
    Closed
  | -- | An opener, spaces aside: an opening bracket, or a leading separator
    -- that keeps what it leads on its line.
    Opened
  deriving (Eq)

-- | A line, and what the engine knew about it as it wrote it.
data Line = Line
  { -- | The indentation it was started at.
    lineIndent :: !Int,
    -- | What it holds after its indentation.
    lineBody :: !Text,
    -- | Whether what it reproduces started it at the margin, whatever the
    -- indentation, and so keeps it there.
    linePinned :: !Bool,
    -- | Whether a note is all it holds.
    lineNote :: !Bool,
    -- | Its type if it is a preprocessor directive.
    lineDirective :: !(Maybe CppDirectiveType),
    -- | Where in its body a line comment held back for its end begins, where
    -- one was and nothing went under it.
    lineRemark :: !(Maybe Int)
  }

-- | The type of a preprocessor directive.
data CppDirectiveType
  = -- | It opens a conditional.
    CppDirectiveOpens
  | -- | It begins another alternative of the conditional it is in.
    CppDirectiveContinues
  | -- | It closes a conditional.
    CppDirectiveCloses
  | -- | It is not a conditional at all.
    CppDirectiveOpaque

-- | A line with nothing on it and nothing known about it.
newLine :: Line
newLine =
  Line
    { lineIndent = 0,
      lineBody = "",
      linePinned = False,
      lineNote = False,
      lineDirective = Nothing,
      lineRemark = Nothing
    }

-- | A line as it is printed.
lineText :: Line -> Text
lineText l
  | T.null (lineBody l) = ""
  | otherwise = T.replicate (lineIndent l) " " <> lineBody l

emptyOut :: Out
emptyOut =
  Out
    { outLines = [],
      outLine = newLine,
      outCurrent = [],
      outColumn = 0,
      outHeldBack = [],
      outRemark = Nothing,
      outCrowded = [],
      outEnding = Written,
      outReferents = HashMap.empty,
      outUnwritten = [],
      outBlank = [],
      outBegun = 0
    }

-- | Turn a document into text.
printDoc ::
  -- | What to print it with.
  Settings ->
  -- | The document.
  Doc ->
  Text
printDoc settings = finish . settled
  where
    settled doc = case outCrowded out of
      [] -> out
      crowded -> settled (endingTheirLines crowded doc)
      where
        out = go env doc emptyOut
    env =
      Env
        { envIndent = 0,
          envBase = 0,
          envWithin = -1,
          envLayout = Broken,
          envNote = False,
          envHeldFlat = False,
          envSettings = settings
        }

-- | Walk a document, accumulating output.
go ::
  -- | What is in force where the document is.
  Env ->
  -- | The document.
  Doc ->
  -- | The output so far.
  Out ->
  Out
go env = \case
  DEmpty -> id
  DText t
    | T.null t -> id
    | otherwise ->
        placed . putText (envIndent env) t . noting (envNote env)
  DSpace -> putSpace
  DBreak -> case envLayout env of
    Broken -> breakLine (envIndent env)
    _ -> putSpace
  DSoftBreak -> case envLayout env of
    Broken -> breakLine (envIndent env)
    _ -> id
  DCloseLine -> closeLine (envIndent env)
  DHardBreak -> breakLine (envIndent env)
  DVerbatimBreak lineStart trailing ->
    verbatimBreakLine (envNote env) lineStart trailing
  DCat a b -> go env b . go env a
  DReferent k d -> \out ->
    let naming = isJust k || printsNothing d
        recorded =
          HashMap.member k (outReferents out) && k `notElem` outBlank out
     in if naming && recorded
          then go env d out
          else
            unplaced k $
              go
                env
                d
                out
                  { outReferents =
                      HashMap.insert k (baseAt env out) (outReferents out),
                    outUnwritten =
                      (k, current out, envIndent env) : outUnwritten out,
                    outBlank = filter (/= k) (outBlank out)
                  }
  DIndented n a d -> \out ->
    let names = referentsIn a
        pointer = namesOnly a && not (null names)
        printed = if pointer then out else go env (unnamed a) out
        base
          | pointer =
              fromMaybe (error "Tilia: dangling pointer") . asum $
                fmap (`HashMap.lookup` outReferents out) names
          | otherwise = baseAt env out
        aliased = case names of
          _ : _ : _ ->
            printed
              { outReferents =
                  foldr (`HashMap.insert` base) (outReferents printed) names
              }
          _ -> printed
        column = nestedBy (step (envSettings env)) n base
     in go
          env{envIndent = column, envBase = base, envWithin = current printed}
          d
          aliased
  DAligned n d -> \out ->
    let column = columnAt env out + n
     in go env{envIndent = column, envBase = column, envWithin = -1} d out
  DCppMarginNote d -> \out ->
    go env{envNote = not (hasContent out)} d out
  DGroup l d -> \out ->
    let settled = case l of
          _ | l /= Broken, opensUp d -> Broken
          Frangible
            | not (envHeldFlat env),
              fracturesFrom
                (settingsColumnLimit (envSettings env))
                out
                (envIndent env)
                d ->
                Broken
          _ -> l
     in go env{envLayout = settled, envHeldFlat = settled /= Broken} d out
  DVariant by a b -> go env (chosen by (envLayout env) a b)
  DLocated _ d -> go env d
  DFence _ d -> go env d
  DSeparator LeadingSeparator l@(DCat _ DSpace) _ -> opened . go env l
  DSeparator LeadingSeparator l _ -> go env l
  DSeparator TrailingSeparator _ t -> go env t
  DBracket OpeningBracket _ d -> opened . go env d
  DBracket ClosingBracket _ d -> go env d
  DComment shape c d -> case shape of
    InPlace -> go env d
    EndsTheLine ->
      closeLine (envIndent env) . go env d . putSpace . crowding True
    HeldBack spill -> \out ->
      let held =
            foldl
              ( \o (s, t) ->
                  placed (putHeldBack (envIndent env) s t (noting False o))
              )
              (crowding False out)
              (heldBackLines spill d)
       in if isNothing (outRemark out) && not (null (outHeldBack held))
            then held{outRemark = Just (c, outColumn out)}
            else held
    OnItsOwnLines gap -> \out ->
      let begun
            | outEnding out == Opened && null (outHeldBack out) = putSpace out
            | gap =
                go
                  env
                  (DCloseLine <> DHardBreak <> DHardBreak <> DCloseLine)
                  out
            | otherwise = closeLine (envIndent env) out
       in closeLine (envIndent env) (go env d begun)
  DCppChoice cs branches fallback -> \out ->
    let afterElse = foldMap conditionalElse (listToMaybe cs)
        afterEndif = foldMap conditionalEndif (listToMaybe cs)
        guards = maybe (fmap fst branches) conditionalGuards (listToMaybe cs)
        alternative d o =
          let o' =
                go
                  env{envHeldFlat = False}
                  ( case envLayout env of
                      Frangible -> DGroup Frangible d
                      _ -> d
                  )
                  o
                    { outReferents = outReferents out,
                      outUnwritten = outUnwritten out,
                      outBlank = outBlank out
                    }
              placedIn x =
                HashMap.differenceWith
                  (\new old -> if new == old then Nothing else Just new)
                  (outReferents x)
                  (outReferents out)
           in o'
                { outReferents =
                    HashMap.unions [placedIn o, placedIn o', outReferents out],
                  outUnwritten =
                    filter (`elem` outUnwritten o') (outUnwritten o),
                  outBlank = filter (`elem` outBlank o') (outBlank o)
                }
        steps =
          [ [directive type' ("#" <> guard'), alternative taken]
          | (type', guard', (_, taken)) <- zip3 (CppDirectiveOpens : repeat CppDirectiveContinues) guards branches
          ]
            <> [ [directive CppDirectiveContinues ("#else" <> afterElse), alternative fallback]
               | not (printsNothing fallback && T.null afterElse)
               ]
            <> [[directive CppDirectiveCloses ("#endif" <> afterEndif)]]
     in foldr (flip (.)) id (concat steps) out
  DCppDirective _ t -> directive CppDirectiveOpaque ("#" <> t)
  DDeclarationsStart -> id

-- | Where a step from a part entered here counts from: 'envBase' on the
-- line the innermost relation was entered on, the indentation elsewhere.
baseAt ::
  -- | What is in force.
  Env ->
  -- | The output so far.
  Out ->
  Int
baseAt env out
  | started out, outBegun out == envWithin env = envBase env
  | otherwise = envIndent env

-- | A part with the mark that makes it a referent taken off it.
unnamed ::
  -- | The part.
  Doc ->
  Doc
unnamed = \case
  DReferent _ d -> d
  d -> d

-- | Leave the referent with this region begun where it was entered if
-- nothing was written in it, until a later part of it is.
unplaced ::
  -- | The region.
  Maybe Span ->
  -- | The output so far.
  Out ->
  Out
unplaced k out
  | any (\(k', _, _) -> k' == k) (outUnwritten out) =
      out
        { outUnwritten = filter (\(k', _, _) -> k' /= k) (outUnwritten out),
          outBlank = k : outBlank out
        }
  | otherwise = out

-- | Note that text was written in each referent being written, the base
-- of one that begins a later line than it was entered on becoming the
-- indentation it was entered at.
placed ::
  -- | The output so far.
  Out ->
  Out
placed out = case outUnwritten out of
  [] -> out
  ks ->
    out
      { outReferents = foldr place (outReferents out) ks,
        outUnwritten = []
      }
  where
    place (k, entered, indent)
      | entered == outBegun out = id
      | otherwise = HashMap.adjust (const indent) k

-- | The column what is written next begins at.
columnAt ::
  -- | What is in force.
  Env ->
  -- | The output so far.
  Out ->
  Int
columnAt env out
  | linePinned (outLine out) || started out = outColumn out
  | otherwise = envIndent env

-- | The line being written, counted by 'outBegun', if it has begun.
current ::
  -- | The output so far.
  Out ->
  Int
current out = if started out then outBegun out else -1

-- | Does a frangible document break where it is about to be printed?
fracturesFrom ::
  -- | The column limit.
  Int ->
  -- | The output so far.
  Out ->
  -- | The indentation in force.
  Int ->
  -- | The document.
  Doc ->
  Bool
fracturesFrom limit out indent =
  isJust
    . fracture
      limit
      (if started out then outColumn out else indent)
      (started out && not (endsWithSpace out))

-- | Where the first line of a frangible document laid out broken stops, if
-- it breaks from this column: if it runs past the column limit flat and
-- stops sooner broken.
fracture ::
  -- | The column limit.
  Int ->
  -- | The column it starts at.
  Int ->
  -- | Whether a space asked for first is printed.
  Bool ->
  -- | The document.
  Doc ->
  Maybe (Int, Bool, Bool)
fracture limit column spaceFirst d
  | flat limit > limit, flat end > end = Just broken
  | otherwise = Nothing
  where
    flat bound =
      let (c, _, _) =
            columnReached limit bound column spaceFirst [(Flat, True, d)]
       in c
    broken@(end, _, _) =
      columnReached limit maxBound column spaceFirst [(Broken, False, d)]

-- | The column a line reaches, or the first one past the bound, whether a
-- space asked for next is printed there, and whether the line ends there
-- rather than what is laid out on it.
columnReached ::
  -- | The column limit.
  Int ->
  -- | The column past which it stops.
  Int ->
  -- | The column it starts at.
  Int ->
  -- | Whether a space asked for first is printed.
  Bool ->
  -- | What is left to lay out, each with the layout in force and whether a
  -- flat group around it holds it, even past a broken one.
  [(Layout, Bool, Doc)] ->
  (Int, Bool, Bool)
columnReached limit bound = walk
  where
    walk column spaceDue items
      | column > bound = (column, spaceDue, True)
      | otherwise = case items of
          [] -> (column, spaceDue, False)
          (layout, held, x) : rest -> case x of
            DText t
              | T.null t -> walk column spaceDue rest
              | otherwise -> walk (column + T.length t) True rest
            DSpace -> space column spaceDue rest
            DBreak -> case layout of
              Broken -> (column, spaceDue, True)
              _ -> space column spaceDue rest
            DSoftBreak -> case layout of
              Broken -> (column, spaceDue, True)
              _ -> walk column spaceDue rest
            DCat a b ->
              walk
                column
                spaceDue
                ((layout, held, a) : (layout, held, b) : rest)
            DIndented _ a b ->
              walk
                column
                spaceDue
                ((layout, held, a) : (layout, held, b) : rest)
            DGroup l y
              | l /= Broken,
                opensUp y ->
                  walk column spaceDue ((Broken, held, y) : rest)
            DGroup Frangible y
              | not held -> case fracture limit column spaceDue y of
                  Just (c, s, True) -> (c, s, True)
                  Just (c, s, False) -> walk c s rest
                  Nothing -> walk column spaceDue ((Flat, True, y) : rest)
            DGroup l y ->
              let settled = if l == Broken then Broken else Flat
               in walk
                    column
                    spaceDue
                    ((settled, held || settled == Flat, y) : rest)
            DVariant by a b ->
              walk column spaceDue ((layout, held, chosen by layout a b) : rest)
            DHardBreak -> (column, spaceDue, True)
            DCloseLine -> (column, spaceDue, True)
            DComment shape _ y -> case shape of
              InPlace -> walk column spaceDue ((layout, held, y) : rest)
              EndsTheLine ->
                space
                  column
                  spaceDue
                  ((layout, held, y) : (layout, held, DCloseLine) : rest)
              HeldBack _ -> walk column spaceDue rest
              OnItsOwnLines _ -> (column, spaceDue, True)
            DVerbatimBreak{} -> (column, spaceDue, True)
            DCppChoice{} -> (column, spaceDue, True)
            DCppDirective{} -> (column, spaceDue, True)
            DEmpty -> walk column spaceDue rest
            DDeclarationsStart -> walk column spaceDue rest
            _
              | Just (_, y) <- unwrap x ->
                  walk column spaceDue ((layout, held, y) : rest)
              | otherwise -> (column, spaceDue, True)
    space column spaceDue rest
      | spaceDue = walk (column + 1) False rest
      | otherwise = walk column spaceDue rest

-- | Record whether the line holds nothing but a note, given whether what is
-- about to be written to it is one.
noting :: Bool -> Out -> Out
noting note out
  | note' == lineNote l = out
  | otherwise = out{outLine = l{lineNote = note'}}
  where
    l = outLine out
    note' = note && (not (started out) || lineNote l)

-- | Put a directive at the margin, on a line of its own, with no empty line
-- above one that ends a branch but the one cpp joins to a line ending in a
-- backslash.
directive :: CppDirectiveType -> Text -> Out -> Out
directive type' t =
  closeLine 0 . typed . putText 0 t . endingBranch . closeLine 0
  where
    typed out =
      out{outLine = (outLine out){lineNote = False, lineDirective = Just type'}}
    endingBranch out = case type' of
      CppDirectiveContinues -> unspaced out
      CppDirectiveCloses -> unspaced out
      _ -> out
    unspaced out = out{outLines = unspacedLines (outLines out)}
    unspacedLines ls = case span (T.null . lineBody) ls of
      (blank : _, l : rest) | endsInBackslash (lineBody l) -> blank : l : rest
      (_, rest) -> rest

-- | Append a fragment, starting the line at the indentation if this is the
-- first thing on it.
putText :: Int -> Text -> Out -> Out
putText indent t out0
  | T.null t = out0
  | started out =
      out
        { outCurrent = t : outCurrent out,
          outColumn = outColumn out + T.length t
        }
  | otherwise =
      out
        { outCurrent = [t],
          outColumn = indent + T.length t,
          outLine = (outLine out){lineIndent = indent}
        }
  where
    out = out0{outEnding = Written}

-- | Hold a fragment back until the line ends.
putHeldBack :: Int -> Spill -> Text -> Out -> Out
putHeldBack indent spill t out
  | started out || not (null (outHeldBack out)) =
      out{outHeldBack = outHeldBack out <> [(spill, t)], outEnding = Written}
  | otherwise = closeLine indent (putText indent t out)

-- | Append a space, unless the line has not started or already ends in one.
putSpace :: Out -> Out
putSpace out
  | not (started out) = out
  | endsWithSpace out = out
  | otherwise =
      out
        { outCurrent = " " : outCurrent out,
          outColumn = outColumn out + 1
        }

-- | Does the current line already end in a space?
endsWithSpace :: Out -> Bool
endsWithSpace out = case outCurrent out of
  (t : _) -> maybe False ((== ' ') . snd) (T.unsnoc t)
  [] -> False

-- | Finish the current line.
breakLine ::
  -- | Where the line after this one begins.
  Int ->
  -- | The output so far.
  Out ->
  Out
breakLine indent out
  | outEnding out == Closed = out{outEnding = Written}
  | not (hasContent out), repeatsBlank out || opensABlock indent out = fresh out
  | otherwise = completing TrimWhitespace out

-- | Close the current line, if there is anything on it.
--
-- Unlike 'breakLine' this leaves a mark: the next break sees that the line
-- was already ended on purpose and does nothing, so a comment that ends its
-- own line and a construct that would have ended it anyway do not between
-- them leave an empty one.
closeLine ::
  -- | Where the line after this one begins.
  Int ->
  Out ->
  Out
closeLine indent out
  | hasContent out = (breakLine indent out){outEnding = Closed}
  | otherwise = out

-- | Begin a new line, leaving the one being built unfinished.
fresh :: Out -> Out
fresh out =
  out
    { outBegun = outBegun out + 1,
      outLine = newLine,
      outCurrent = [],
      outColumn = 0,
      outHeldBack = [],
      outRemark = Nothing,
      outEnding = Written
    }

-- | Finish the current line, with the held-back fragments that spill under
-- it, and begin a new one.
completing ::
  -- | What to do with whitespace the lines end in.
  TrailingWhitespace ->
  -- | The output so far.
  Out ->
  Out
completing trailing out =
  fresh out{outLines = reverse (finished : spilledLines) <> outLines out}
  where
    finished = (outLine out){lineBody = body, lineRemark = remark}
    body
      | T.null written = heldBack
      | T.null heldBack = written
      | otherwise = written <> " " <> heldBack
    written =
      adjustTrailingWhitespace trailing (T.concat (reverse (outCurrent out)))
    heldBack =
      maybe
        ""
        (adjustTrailingWhitespace trailing . snd)
        (listToMaybe (outHeldBack out))
    heldAt = T.length body - T.length heldBack
    remark
      | [_] <- outHeldBack out,
        heldAt > 0,
        "--" `T.isPrefixOf` heldBack =
          Just heldAt
      | otherwise = Nothing
    spilled = drop 1 (outHeldBack out)
    spilledLines = zipWith below columns spilled
    columns =
      drop 1 (scanl columnFor (lineIndent finished + heldAt) (fmap fst spilled))
    below column (spill, t) =
      newLine
        { lineIndent = if spill == SpillAtMargin then 0 else column,
          lineBody = adjustTrailingWhitespace trailing t
        }
    columnFor previous = \case
      SpillAtIndentation -> lineIndent finished + commaLed body
      SpillUnderPrevious -> previous
      SpillAtMargin -> previous

-- | The width of the leading comma a line begins with, if it begins with
-- one, which what lines up with the line lines up past.
commaLed ::
  -- | What the line holds after its indentation.
  Text ->
  Int
commaLed body = if ", " `T.isPrefixOf` body then 2 else 0

-- | Would an empty line here be the first thing inside a block?
opensABlock :: Int -> Out -> Bool
opensABlock indent out = case outLines out of
  (l : _) -> isNothing (lineDirective l) && T.length (lineText l) <= indent
  [] -> False

-- | Finish the current line between two lines of reproduced text.
verbatimBreakLine ::
  -- | Whether the next line is part of a note.
  Bool ->
  LineStart ->
  TrailingWhitespace ->
  Out ->
  Out
verbatimBreakLine note lineStart trailing out =
  (completing trailing out)
    { outLine = newLine{linePinned = lineStart == AtMargin, lineNote = note},
      outEnding = Written
    }

-- | Would this empty line be the first line or a second empty one in a
-- row?
repeatsBlank ::
  -- | The output so far.
  Out ->
  Bool
repeatsBlank out = case outLines out of
  (l : _) -> T.null (lineBody l)
  [] -> True

-- | Has the current line begun, with something written to it or with what
-- it reproduces starting it at the margin?
started :: Out -> Bool
started out = not (null (outCurrent out)) || linePinned (outLine out)

-- | Does a comment end a line inside a bracket that a document opens, with
-- more of the document to follow, where it is laid out flat?
--
-- Only the document's own level counts: a group inside it settles that
-- for itself.
opensUp ::
  -- | The document.
  Doc ->
  Bool
opensUp d = go' (0 :: Int) False [d]
  where
    go' depth ended = \case
      [] -> False
      x : rest -> case x of
        DText t
          | T.null t -> go' depth ended rest
          | otherwise -> ended || go' depth ended rest
        DBracket side _ _ ->
          ended
            || go'
              (if side == OpeningBracket then depth + 1 else depth - 1)
              ended
              rest
        DComment shape _ _ -> case shape of
          EndsTheLine -> go' depth (ended || depth > 0) rest
          OnItsOwnLines _ -> go' depth (ended || depth > 0) rest
          InPlace -> ended || go' depth ended rest
          HeldBack _ -> go' depth ended rest
        DGroup _ _ -> ended || go' depth ended rest
        DCat a b -> go' depth ended (a : b : rest)
        DIndented _ a b -> go' depth ended (a : b : rest)
        DVariant by a b -> go' depth ended (chosen by Flat a b : rest)
        DHardBreak -> go' depth False rest
        DCloseLine -> go' depth False rest
        DVerbatimBreak{} -> go' depth False rest
        DCppChoice{} -> go' depth False rest
        DCppDirective{} -> go' depth False rest
        _
          | Just (_, y) <- unwrap x -> go' depth ended (y : rest)
          | otherwise -> go' depth ended rest

-- | The layout of a variant that is printed in a group of the given layout.
chosen ::
  -- | What chooses between them.
  ChosenBy ->
  -- | The layout of the group.
  Layout ->
  -- | The first layout.
  Doc ->
  -- | The second layout.
  Doc ->
  Doc
chosen by layout a b = case by of
  ByLayout
    | layout == Broken -> b
    | otherwise -> a
  BySeparators
    | separatorAfterComment a -> b
    | otherwise -> a

-- | Does a comment end a line right before a separator of the document's
-- own that trails the part before it, which would then begin a line?
--
-- A region holds a part, whose separators are its own concern.
separatorAfterComment ::
  -- | The document.
  Doc ->
  Bool
separatorAfterComment d = go' False False [d]
  where
    go' separating ended = \case
      [] -> False
      x : rest -> case x of
        DText t
          | T.null t -> go' separating ended rest
          | otherwise -> separating && ended || go' False False rest
        DSeparator TrailingSeparator _ t -> go' True ended (t : rest)
        DComment shape _ c -> case shape of
          EndsTheLine -> go' separating True rest
          OnItsOwnLines _ -> go' separating True rest
          InPlace -> go' separating ended (c : rest)
          HeldBack _ -> go' separating ended rest
        DLocated _ y
          | separating -> go' separating ended (y : rest)
          | otherwise -> go' False (ended && printsNothing y) rest
        DVariant BySeparators _ _ -> go' False False rest
        DVariant ByLayout a _ -> go' separating ended (a : rest)
        DCat a b -> go' separating ended (a : b : rest)
        DIndented _ a b -> go' separating ended (a : b : rest)
        DCppChoice{} -> go' False False rest
        DCppDirective{} -> go' False False rest
        _
          | Just (_, y) <- unwrap x -> go' separating ended (y : rest)
          | otherwise -> go' separating ended rest

-- | Record the remark held back for the line as crowded where a comment
-- about to join the line would come before it: after code written after
-- it, or anywhere if the comment ends the line.
crowding ::
  -- | Does the comment end the line?
  Bool ->
  -- | The output so far.
  Out ->
  Out
crowding ends out = case outRemark out of
  Just (r, column)
    | ends || outColumn out > column ->
        out{outCrowded = r : outCrowded out, outRemark = Nothing}
  _ -> out

-- | The document with each of these remarks ending its line where it
-- stands rather than held back to the end of it.
endingTheirLines ::
  -- | Where the remarks were written.
  [Span] ->
  -- | The document.
  Doc ->
  Doc
endingTheirLines crowded = go'
  where
    go' = \case
      DComment (HeldBack _) c d | c `elem` crowded -> DComment EndsTheLine c d
      DCat a b -> DCat (go' a) (go' b)
      DIndented n a b -> DIndented n (go' a) (go' b)
      DVariant by a b -> DVariant by (go' a) (go' b)
      DCppChoice cs bs e -> DCppChoice cs [(t, go' d) | (t, d) <- bs] (go' e)
      d
        | Just (w, x) <- unwrap d -> wrap w (go' x)
        | otherwise -> d

-- | Note that the line ends with an opener.
opened :: Out -> Out
opened out = out{outEnding = Opened}

-- | The lines of a comment to hold back for the end of a line, each with
-- where it goes once it spills under that line: at the margin where a break
-- to the margin begins it, and as told otherwise.
heldBackLines ::
  -- | Where a line that spills goes.
  Spill ->
  -- | The comment.
  Doc ->
  [(Spill, Text)]
heldBackLines spill d = reverse (collect d [(spill, "")])
  where
    collect = \case
      DText t -> \case
        (s, l) : ls -> (s, l <> t) : ls
        [] -> [(spill, t)]
      DVerbatimBreak AtMargin _ -> ((SpillAtMargin, "") :)
      DVerbatimBreak AtIndent _ -> ((spill, "") :)
      DCat a b -> collect b . collect a
      x -> maybe id (collect . snd) (unwrap x)

-- | Is there anything on the current line, written or held back?
hasContent :: Out -> Bool
hasContent out = started out || not (null (outHeldBack out))

-- | Handle the given line according to the 'TrailingWhitespace' style.
adjustTrailingWhitespace :: TrailingWhitespace -> Text -> Text
adjustTrailingWhitespace = \case
  TrimWhitespace -> T.stripEnd
  KeepWhitespace -> id

-- | Assemble the final text: one trailing newline, no blank lines at the
-- end, and trailing whitespace only where a verbatim line keeps it.
finish ::
  -- | What the walk wrote.
  Out ->
  Text
finish out =
  case dropWhile (T.null . lineBody) (outLines (breakLine 0 out)) of
    [] -> ""
    ls ->
      T.unlines . fmap lineText $
        remarksCarriedOn (notesToMargin (reverse ls))

-- | Put the comments right under a line that ends with a remark at the
-- remark's column where they would begin right of it, which is where a
-- comment carrying the remark on goes and how they are read again.
remarksCarriedOn ::
  -- | The lines.
  [Line] ->
  [Line]
remarksCarriedOn = \case
  l : ls
    | Just at <- lineRemark l,
      not (linePinned l) ->
        let column = lineIndent l + at
            (under, rest) = span (rightOf column) ls
         in l
              : fmap (\c -> c{lineIndent = column}) under
                <> remarksCarriedOn rest
  l : ls -> l : remarksCarriedOn ls
  [] -> []
  where
    rightOf column c =
      not (linePinned c)
        && lineIndent c > column
        && maybe False plain (commentOpening (lineBody c))
    plain =
      maybe True ((`notElem` ['|', '^', '*', '$']) . fst)
        . T.uncons
        . T.stripStart

-- | What follows the opening of a comment that text begins with, rather
-- than an operator made of dashes or a pragma, if it begins with one.
commentOpening ::
  -- | The text.
  Text ->
  Maybe Text
commentOpening t
  | opensLineComment t = Just (T.dropWhile (== '-') t)
  | Just inside <- T.stripPrefix "{-" t,
    not ("#" `T.isPrefixOf` inside) =
      Just inside
  | otherwise = Nothing

-- | Move every note right above a directive to the margin.
notesToMargin :: [Line] -> [Line]
notesToMargin = fst . foldr place ([], False)
  where
    place l (ls, above)
      | Just _ <- lineDirective l = (l : ls, True)
      | above, lineNote l = (l{lineIndent = 0} : ls, True)
      | otherwise = (l : ls, False)
