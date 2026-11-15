{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The document representation and the engine that turns it into text.
module Tilia.Doc.Internal
  ( -- * Documents
    Doc (..),
    Wrapper (..),
    unwrap,
    wrap,
    layoutInside,
    Nesting (..),
    Spill (..),
    printsNothing,
    Conditional (..),
    conditionalRange,
    Layout (..),
    LineStart (..),
    TrailingWhitespace (..),

    -- * Rendering
    printDoc,
  )
where

import Data.Char (isSpace)
import Data.List (unsnoc)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

import Tilia.Settings (Settings (..), indentationColumns)
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
  | -- | Text to be put at the end of the line this position falls on,
    -- however much of the line is still to be written.
    DHoldBack !Spill !Text
  | -- | Close the line, and let a break that immediately follows know that
    -- it has nothing left to do.
    DCloseLine
  | -- | Close the line, and leave an empty line under it if told to, unless
    -- it ends with an opening bracket, a bar, an equals sign or a leading
    -- comma, which what follows then shares.
    DCloseLineUnlessAfterOpener !Bool
  | -- | A line break between two lines of text that is being reproduced
    -- rather than laid out.
    DVerbatimBreak !LineStart !TrailingWhitespace
  | -- | Concatenation. See the 'Semigroup' instance.
    DCat !Doc !Doc
  | -- | Indent the enclosed document relative to the current indentation.
    DNest !Nesting !Doc
  | -- | Indent the enclosed document to whatever column the line has
    -- reached, so that it lines up under itself when broken.
    DAlign !Doc
  | -- | Start the enclosed document's lines at the margin if they come out
    -- right above a preprocessor directive.
    DCppMarginNote !Doc
  | -- | Lay the enclosed document out flat or broken.
    DGroup !Layout !Doc
  | -- | Choose between two documents according to the enclosing group: the
    -- first when it is flat, the second when it is broken. For the
    -- constructs whose two layouts differ by more than where the breaks
    -- fall.
    --
    -- Both fields are lazy, and that is not an oversight. The engine walks
    -- one of them and never looks at the other, so the branch not taken
    -- should cost nothing. Were they strict, building a variant would build
    -- both layouts of everything inside it; a construct nested @n@ deep
    -- would be built @2^n@ times.
    DVariant Doc Doc
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

-- | Does this document put nothing at all on the page?
printsNothing ::
  -- | The document.
  Doc ->
  Bool
printsNothing = \case
  DEmpty -> True
  DDeclarationsStart -> True
  DCat a b -> printsNothing a && printsNothing b
  DVariant flatD brokenD -> printsNothing flatD && printsNothing brokenD
  d -> maybe False (printsNothing . snd) (unwrap d)

-- | A node holding exactly one document, less the document.
data Wrapper
  = WLocated !Span
  | WFence !Span
  | WNest !Nesting
  | WAlign
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
  DNest n d -> Just (WNest n, d)
  DAlign d -> Just (WAlign, d)
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
  WNest n -> DNest n
  WAlign -> DAlign
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

-- | A conditional as its author wrote it.
data Conditional = Conditional
  { -- | The lines of its directives, the @#if@ first and the @#endif@ last.
    conditionalLines :: [Int],
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

-- | Where the line after a 'DVerbatimBreak' begins.
data LineStart
  = -- | At the indentation in force, as any other break would.
    AtIndent
  | -- | At column zero, whatever the indentation.
    AtMargin
  deriving (Eq, Show)

-- | What to do with the whitespace a finished line ends in.
data TrailingWhitespace
  = -- | Trim it.
    TrimWhitespace
  | -- | Keep it.
    KeepWhitespace
  deriving (Eq, Show)

----------------------------------------------------------------------------
-- Rendering

-- | Columns per indentation step.
step ::
  -- | What the document is printed with.
  Settings ->
  Int
step = indentationColumns . settingsIndentation

-- | The indentation a nesting leads to from another, where a step wider
-- than two columns is taken from the nearest multiple of itself at or left
-- of it, as Fourmolu does.
nestedBy ::
  -- | Columns per indentation step.
  Int ->
  -- | The nesting.
  Nesting ->
  -- | The indentation to step in from.
  Int ->
  Int
nestedBy columns nesting indent = case nesting of
  Steps n
    | columns > 2, n > 0 -> indent `div` columns * columns + n * columns
    | otherwise -> indent + n * columns
  Columns n -> indent + n

-- | What the engine carries while walking a document.
data Env = Env
  { -- | The column a new line starts at.
    envIndent :: !Int,
    -- | The layout of the innermost group.
    envLayout :: !Layout,
    -- | Whether what is written now is a note, which goes to the margin
    -- right above a preprocessor directive.
    envNote :: !Bool,
    -- | Whether a group laid out flat holds what is written now on its
    -- line, which keeps every 'Frangible' group in it flat.
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
    -- | Whether the line was closed by something that already knew it was
    -- ending it, so that a break arriving now would add an empty line
    -- rather than end anything.
    outClosed :: !Bool
  }

-- | How far 'DNest' moves the indentation.
data Nesting
  = -- | So many indentation steps, as many columns each as the project asks.
    Steps !Int
  | -- | So many columns, whatever the indentation step.
    Columns !Int
  deriving (Eq, Show)

-- | Where text held back for the end of a line goes when something was held
-- back for it first.
data Spill
  = -- | On a line of its own, at the indentation of the line it was held
    -- back for.
    SpillAtIndentation
  | -- | On a line of its own, lined up with what was held back before it.
    SpillUnderPrevious
  deriving (Eq, Show)

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
    -- | Whether it continues the line before it, as a space asked for before
    -- anything was on it says.
    lineContinues :: !Bool,
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
      lineContinues = False,
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
      outClosed = False
    }

-- | Turn a document into text.
printDoc ::
  -- | What to print it with.
  Settings ->
  -- | The document.
  Doc ->
  Text
printDoc settings doc = finish (step settings) (go env doc emptyOut)
  where
    env =
      Env
        { envIndent = 0,
          envLayout = Broken,
          envNote = False,
          envHeldFlat = False,
          envSettings = settings
        }

-- | Walk a document, accumulating output.
go :: Env -> Doc -> Out -> Out
go env = \case
  DEmpty -> id
  DText t
    | T.null t -> id
    | otherwise -> putText (envIndent env) t . noting (envNote env)
  DSpace -> putSpace
  DBreak -> case envLayout env of
    Broken -> breakLine (envIndent env)
    _ -> putSpace
  DSoftBreak -> case envLayout env of
    Broken -> breakLine (envIndent env)
    _ -> id
  DHoldBack spill t -> putHeldBack (envIndent env) spill t . noting False
  DCloseLine -> closeLine (envIndent env)
  DCloseLineUnlessAfterOpener gap -> \out ->
    if endsWithOpener out
      then putSpace out
      else go env (gapped <> DCloseLine) out
    where
      gapped = if gap then DCloseLine <> DHardBreak <> DHardBreak else DEmpty
  DHardBreak -> breakLine (envIndent env)
  DVerbatimBreak lineStart trailing ->
    verbatimBreakLine (envNote env) lineStart trailing
  DCat a b -> go env b . go env a
  DNest n d ->
    go env{envIndent = nestedBy (step (envSettings env)) n (envIndent env)} d
  DAlign d -> \out ->
    let column
          | linePinned (outLine out) = outColumn out
          | otherwise = max (envIndent env) (outColumn out)
     in go env{envIndent = column} d out
  DCppMarginNote d -> \out ->
    go env{envNote = not (hasContent out)} d out
  DGroup l d -> \out ->
    let settled = case l of
          Frangible
            | not (envHeldFlat env),
              fracturesFrom
                (settingsColumnLimit (envSettings env))
                out
                (envIndent env)
                d ->
                Broken
            | otherwise -> Flat
          _ -> l
        heldFlat = envHeldFlat env || settled == Flat
     in go env{envLayout = settled, envHeldFlat = heldFlat} d out
  DVariant flatD brokenD -> case envLayout env of
    Broken -> go env brokenD
    _ -> go env flatD
  DLocated _ d -> go env d
  DFence _ d -> go env d
  DCppChoice cs branches fallback ->
    let afterElse = foldMap conditionalElse (listToMaybe cs)
        afterEndif = foldMap conditionalEndif (listToMaybe cs)
        unheld = env{envHeldFlat = False}
     in foldr (flip (.)) id . concat $
          [ [directive type' ("#" <> guard'), go unheld taken]
          | (type', (guard', taken)) <- zip (CppDirectiveOpens : repeat CppDirectiveContinues) branches
          ]
            <> [ [directive CppDirectiveContinues ("#else" <> afterElse), go unheld fallback]
               | not (printsNothing fallback && T.null afterElse)
               ]
            <> [[directive CppDirectiveCloses ("#endif" <> afterEndif)]]
  DCppDirective _ t -> directive CppDirectiveOpaque ("#" <> t)
  DDeclarationsStart -> id

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
  -- flat group holds it.
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
            DVariant flatD brokenD -> case layout of
              Broken -> walk column spaceDue ((layout, held, brokenD) : rest)
              _ -> walk column spaceDue ((layout, held, flatD) : rest)
            DHardBreak -> (column, spaceDue, True)
            DCloseLine -> (column, spaceDue, True)
            DCloseLineUnlessAfterOpener _ -> (column, spaceDue, True)
            DVerbatimBreak{} -> (column, spaceDue, True)
            DCppChoice{} -> (column, spaceDue, True)
            DCppDirective{} -> (column, spaceDue, True)
            DEmpty -> walk column spaceDue rest
            DHoldBack{} -> walk column spaceDue rest
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
-- above one that ends a branch.
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
    unspaced out = out{outLines = dropWhile (T.null . lineBody) (outLines out)}

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
    out = out0{outClosed = False}

-- | Hold a fragment back until the line ends.
putHeldBack :: Int -> Spill -> Text -> Out -> Out
putHeldBack indent spill t out
  | started out || not (null (outHeldBack out)) =
      out{outHeldBack = outHeldBack out <> [(spill, t)], outClosed = False}
  | otherwise = closeLine indent (putText indent t out)

-- | Append a space, unless the line has not started or already ends in one.
putSpace :: Out -> Out
putSpace out
  | not (started out) =
      if lineContinues (outLine out)
        then out
        else out{outLine = (outLine out){lineContinues = True}}
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
  | outClosed out = discontinued out{outClosed = False}
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
  | hasContent out = (breakLine indent out){outClosed = True}
  | otherwise = discontinued out

-- | Say that the current line does not continue the line before it.
discontinued :: Out -> Out
discontinued out
  | lineContinues (outLine out) =
      out{outLine = (outLine out){lineContinues = False}}
  | otherwise = out

-- | Begin a new line, leaving the one being built unfinished.
fresh :: Out -> Out
fresh out =
  out
    { outLine = newLine,
      outCurrent = [],
      outColumn = 0,
      outHeldBack = []
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
    below column (_, t) =
      newLine
        { lineIndent = column,
          lineBody = adjustTrailingWhitespace trailing t
        }
    columnFor previous = \case
      SpillAtIndentation -> lineIndent finished + commaLed body
      SpillUnderPrevious -> previous

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
  (l : _) -> T.length (lineText l) <= indent
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
      outClosed = False
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

-- | Does the current line end with an opening bracket, a bar, an equals
-- sign or a leading comma, with nothing held back for its end?
endsWithOpener :: Out -> Bool
endsWithOpener out =
  null (outHeldBack out)
    && case dropWhile (T.all isSpace) (outCurrent out) of
      t : _ -> t == ", " || T.strip t `elem` ["(", "(#", "=", "[", "{", "|"]
      [] -> False

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
  -- | Columns per indentation step.
  Int ->
  -- | What the walk wrote.
  Out ->
  Text
finish columns out =
  case dropWhile (T.null . lineBody) (outLines (breakLine 0 out)) of
    [] -> ""
    ls ->
      T.unlines . fmap lineText $
        remarksCarriedOn (notesToMargin (continued columns (reverse ls)))

-- | Indent each alternative of a conditional that continues the line before
-- the conditional further than that line.
continued ::
  -- | Columns per indentation step.
  Int ->
  -- | The lines.
  [Line] ->
  [Line]
continued columns = walk [] 0 False
  where
    walk frames previous pending = \case
      [] -> []
      l : ls -> case lineDirective l of
        Just CppDirectiveOpens -> l : walk ((previous, 0) : frames) 0 True ls
        Just CppDirectiveContinues -> l : walk (restarted frames) 0 True ls
        Just CppDirectiveCloses -> l : walk (drop 1 frames) 0 False ls
        Just CppDirectiveOpaque -> l : walk frames 0 False ls
        Nothing ->
          let frames' = if pending then settled frames l else frames
              l' = moved (sum (fmap snd frames')) l
           in l' : walk frames' (lineIndent l') False ls
    restarted = \case
      (before, _) : rest -> (before, 0) : rest
      [] -> []
    settled frames l = case frames of
      (before, _) : rest
        | lineContinues l,
          not (linePinned l),
          at <= before ->
            (before, before - at + columns) : rest
        where
          at = lineIndent l + sum (fmap snd rest)
      _ -> frames
    moved n l
      | n == 0 || linePinned l || T.null (lineBody l) = l
      | otherwise = l{lineIndent = lineIndent l + n}

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
