{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The document representation and the engine that turns it into text.
module Tilia.Doc.Internal
  ( -- * Documents
    Doc (..),
    Conditional (..),
    printsNothing,
    Layout (..),
    LineStart (..),
    TrailingWhitespace (..),
    groupLayout,

    -- * Rendering
    RenderOptions (..),
    defaultRenderOptions,
    render,
  )
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Span (Span, isSingleLine)

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
    DHoldBack !Text
  | -- | Close the line, and let a break that immediately follows know that
    -- it has nothing left to do.
    DCloseLine
  | -- | A line break between two lines of text that is being reproduced
    -- rather than laid out.
    DVerbatimBreak !LineStart !TrailingWhitespace
  | -- | Concatenation. See the 'Semigroup' instance.
    DCat !Doc !Doc
  | -- | Indent the enclosed document by the given number of steps, relative
    -- to the current indentation.
    DNest !Int !Doc
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
  deriving (Eq, Show)

-- | A conditional as its author wrote it: the lines of its directives, the
-- @#if@ first and the @#endif@ last.
newtype Conditional = Conditional{conditionalLines :: [Int]}
  deriving (Eq, Ord, Show)

-- | Does this document put nothing at all on the page?
printsNothing :: Doc -> Bool
printsNothing = \case
  DEmpty -> True
  DCat a b -> printsNothing a && printsNothing b
  DNest _ d -> printsNothing d
  DAlign d -> printsNothing d
  DCppMarginNote d -> printsNothing d
  DGroup _ d -> printsNothing d
  DLocated _ d -> printsNothing d
  DFence _ d -> printsNothing d
  DVariant flatD brokenD -> printsNothing flatD && printsNothing brokenD
  _ -> False

instance Semigroup Doc where
  DEmpty <> b = b
  a <> DEmpty = a
  a <> b = DCat a b

instance Monoid Doc where
  mempty = DEmpty

-- | Whether a group is laid out on one line or across several.
data Layout
  = Flat
  | Broken
  deriving (Eq, Show)

-- | Where the line after a 'DVerbatimBreak' begins.
data LineStart
  = -- | At the indentation in force, as any other break would.
    AtIndent
  | -- | At column zero, whatever the indentation.
    AtMargin
  deriving (Eq, Show)

-- | What do to with the whitespace a finished line ends in.
data TrailingWhitespace
  = -- | Trim it.
    TrimWhitespace
  | -- | Keep it.
    KeepWhitespace
  deriving (Eq, Show)

-- | Decide how to lay a group out.
groupLayout :: Maybe Span -> Layout
groupLayout = \case
  Nothing -> Flat
  Just s
    | isSingleLine s -> Flat
    | otherwise -> Broken

----------------------------------------------------------------------------
-- Rendering

-- | Knobs for 'render'.
newtype RenderOptions = RenderOptions
  { -- | Columns per indentation step.
    roIndentStep :: Int
  }
  deriving (Eq, Show)

-- | Two columns per step.
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions{roIndentStep = 2}

-- | What the engine carries while walking a document.
data Env = Env
  { -- | The column a new line starts at.
    envIndent :: !Int,
    -- | The layout of the innermost group.
    envLayout :: !Layout,
    -- | Columns per indentation step.
    envIndentStep :: !Int,
    -- | Whether a line started now should start at the margin.
    envCppMarginNote :: !Bool
  }

-- | Output built so far.
--
-- Lines are finished one at a time and never revisited, so the current line
-- is kept as a reversed list of fragments and completed lines as a reversed
-- list of lines.
data Out = Out
  { -- | Completed lines, most recent first.
    outLines :: [Text],
    -- | Fragments of the line being built, most recent first.
    outCurrent :: [Text],
    -- | Column the current line has reached.
    outColumn :: !Int,
    -- | Whether anything has been written to the current line. Indentation
    -- is emitted lazily, when the first fragment arrives, so that a line
    -- with nothing on it stays genuinely empty.
    outStarted :: !Bool,
    -- | Fragments held back until the line ends, in the order they were
    -- given. The first goes at the end of the line; any after it get lines
    -- of their own under it.
    outHeldBack :: ![Text],
    -- | Whether the line was closed by something that already knew it was
    -- ending it, so that a break arriving now would add an empty line
    -- rather than end anything.
    outClosed :: !Bool,
    -- | The indentation of the current line, if a 'DCppMarginNote' began it
    -- and nothing else is on it.
    outNoteIndent :: !(Maybe Int),
    -- | The indentation of each of the lines just completed that could go
    -- to column zero, most recent first.
    outNoteRun :: ![Int]
  }

emptyOut :: Out
emptyOut =
  Out
    { outLines = [],
      outCurrent = [],
      outColumn = 0,
      outStarted = False,
      outHeldBack = [],
      outClosed = False,
      outNoteIndent = Nothing,
      outNoteRun = []
    }

-- | Turn a document into text.
render :: RenderOptions -> Doc -> Text
render opts doc = finish (go env doc emptyOut)
  where
    env =
      Env
        { envIndent = 0,
          envLayout = Broken,
          envIndentStep = roIndentStep opts,
          envCppMarginNote = False
        }

-- | Walk a document, accumulating output.
go :: Env -> Doc -> Out -> Out
go env = \case
  DEmpty -> id
  DText t
    | T.null t -> id
    | envCppMarginNote env -> \out ->
        (putText (envIndent env) t out)
          { outNoteIndent =
              if outStarted out
                then outNoteIndent out
                else Just (envIndent env)
          }
    | otherwise -> putText (envIndent env) t . unnoted
  DSpace -> putSpace
  DBreak -> case envLayout env of
    Flat -> putSpace
    Broken -> breakLine (envIndent env)
  DSoftBreak -> case envLayout env of
    Flat -> id
    Broken -> breakLine (envIndent env)
  DHoldBack t -> putHeldBack (envIndent env) t . unnoted
  DCloseLine -> closeLine (envIndent env)
  DHardBreak -> breakLine (envIndent env)
  DVerbatimBreak lineStart trailing -> \out ->
    let out' = verbatimBreakLine lineStart trailing out
     in if envCppMarginNote env
          then out'{outNoteIndent = Just (if lineStart == AtMargin then 0 else envIndent env)}
          else out'
  DCat a b -> go env b . go env a
  DNest n d -> go env{envIndent = envIndent env + n * envIndentStep env} d
  DAlign d -> \out ->
    go env{envIndent = max (envIndent env) (outColumn out)} d out
  DCppMarginNote d -> \out ->
    go env{envCppMarginNote = not (hasContent out)} d out
  DGroup l d -> go env{envLayout = l} d
  DVariant flatD brokenD -> case envLayout env of
    Flat -> go env flatD
    Broken -> go env brokenD
  DLocated _ d -> go env d
  DFence _ d -> go env d
  DCppChoice _ branches fallback -> \out ->
    let alternative x = case spacedTextAt env x of
          Just at | at <= continuing -> shifted (continuing - at) x
          _ -> go env x
        shifted k = go env{envIndent = envIndent env + k + envIndentStep env}
        continuing
          | hasContent out = indentationOf (T.concat (reverse (outCurrent out)))
          | otherwise = maybe 0 indentationOf (listToMaybe (outLines out))
        indentationOf = T.length . T.takeWhile (== ' ')
     in ( foldr (flip (.)) id . concat $
            [ [atMargin ("#" <> guard'), alternative taken]
            | (guard', taken) <- branches
            ]
              <> [[atMargin "#else", alternative fallback] | not (printsNothing fallback)]
              <> [[atMargin "#endif"]]
        )
          out
  DCppDirective _ t -> atMargin ("#" <> t)

-- | Where a document that begins with a space puts its first text on a
-- line of its own, or 'Nothing' if it begins otherwise.
spacedTextAt :: Env -> Doc -> Maybe Int
spacedTextAt env0 = either (const Nothing) id . walk env0 False
  where
    -- 'Left' while nothing but spaces has been printed, saying whether a
    -- space has; 'Right' with the answer once text or a break has.
    walk env spaced = \case
      DEmpty -> Left spaced
      DText t
        | T.null t -> Left spaced
        | otherwise -> text env spaced
      DHoldBack _ -> text env spaced
      DSpace -> Left True
      DBreak -> case envLayout env of
        Flat -> Left True
        Broken -> Right Nothing
      DSoftBreak -> case envLayout env of
        Flat -> Left spaced
        Broken -> Right Nothing
      DCat a b -> either (\spaced' -> walk env spaced' b) Right (walk env spaced a)
      DNest n d -> walk env{envIndent = envIndent env + n * envIndentStep env} spaced d
      DAlign d -> walk env spaced d
      DCppMarginNote d -> walk env spaced d
      DGroup l d -> walk env{envLayout = l} spaced d
      DVariant flatD brokenD -> case envLayout env of
        Flat -> walk env spaced flatD
        Broken -> walk env spaced brokenD
      DLocated _ d -> walk env spaced d
      DFence _ d -> walk env spaced d
      _ -> Right Nothing
    text env spaced = Right (if spaced then Just (envIndent env) else Nothing)

-- | Put a line of text at the margin, on a line of its own, with the notes
-- right above it.
atMargin :: Text -> Out -> Out
atMargin t = closeLine 0 . putText 0 t . notesToMargin . closeLine 0

-- | Move the lines of notes just completed to column zero.
notesToMargin :: Out -> Out
notesToMargin out =
  out
    { outLines = zipWith T.drop (outNoteRun out) noted <> rest,
      outNoteRun = []
    }
  where
    (noted, rest) = splitAt (length (outNoteRun out)) (outLines out)

-- | Say that something other than a note is on the current line.
unnoted :: Out -> Out
unnoted out = out{outNoteIndent = Nothing}

-- | Append a fragment, emitting the line's indentation first if this is the
-- first thing on it.
putText :: Int -> Text -> Out -> Out
putText indent t out0
  | T.null t = out0
  | outStarted out =
      out
        { outCurrent = t : outCurrent out,
          outColumn = outColumn out + T.length t
        }
  | otherwise =
      out
        { outCurrent = [t, T.replicate indent " "],
          outColumn = indent + T.length t,
          outStarted = True
        }
  where
    out = out0{outClosed = False}

-- | Hold a fragment back until the line ends.
putHeldBack :: Int -> Text -> Out -> Out
putHeldBack indent t out
  | outStarted out || not (null (outHeldBack out)) =
      out{outHeldBack = outHeldBack out <> [t], outClosed = False}
  | otherwise = closeLine indent (putText indent t out)

-- | Append a space, unless the line has not started or already ends in one.
putSpace :: Out -> Out
putSpace out
  | not (outStarted out) = out
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
  Out ->
  Out
breakLine indent out
  | outClosed out = out{outClosed = False}
  | atStart out = out
  | not (hasContent out), repeatsBlank out || opensABlock indent out = cleared out
  | otherwise = cleared (completing TrimWhitespace indent out)
  where
    cleared o =
      o{outCurrent = [], outColumn = 0, outStarted = False, outHeldBack = []}

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
  | otherwise = out

-- | Every line the break that has just happened produces.
overflow ::
  -- | What to do with whitespace the lines end in.
  TrailingWhitespace ->
  -- | Where the line after these would begin, used only if there is no line
  -- to take the indentation from.
  Int ->
  Out ->
  [Text]
overflow trailing indent out = reverse (finished : fmap below spilled)
  where
    finished = currentLine trailing out
    spilled = drop 1 (outHeldBack out)
    below t = T.replicate column " " <> adjustTrailingWhitespace trailing t
    column
      | T.null finished = indent
      | otherwise = T.length (T.takeWhile (== ' ') finished)

-- | Move the current line, with the held-back fragments that spill under
-- it, to the completed lines, extending or ending the run of note lines.
completing ::
  -- | What to do with whitespace the lines end in.
  TrailingWhitespace ->
  -- | Where the line after these would begin, used only if there is no line
  -- to take the indentation from.
  Int ->
  Out ->
  Out
completing trailing indent out =
  out
    { outLines = produced <> outLines out,
      outNoteIndent = Nothing,
      outNoteRun = case (produced, outNoteIndent out) of
        ([_], Just k) -> k : outNoteRun out
        _ -> []
    }
  where
    produced = overflow trailing indent out

-- | Would an empty line here be the first thing inside a block?
opensABlock :: Int -> Out -> Bool
opensABlock indent out = case outLines out of
  (l : _) -> T.length l <= indent
  [] -> False

-- | Finish the current line between two lines of reproduced text.
verbatimBreakLine :: LineStart -> TrailingWhitespace -> Out -> Out
verbatimBreakLine lineStart trailing out =
  (completing trailing 0 out)
    { outCurrent = [],
      outColumn = 0,
      outStarted = lineStart == AtMargin,
      outHeldBack = [],
      outClosed = False
    }

-- | Would this empty line be a second one in a row?
repeatsBlank :: Out -> Bool
repeatsBlank out = case outLines out of
  ("" : _) -> True
  _ -> False

-- | Is the output still empty?
atStart :: Out -> Bool
atStart out = null (outLines out) && not (hasContent out)

-- | Is there anything on the current line, written or held back?
hasContent :: Out -> Bool
hasContent out = outStarted out || not (null (outHeldBack out))

-- | The current line: what was written to it, then whatever was held back
-- for its end, with one space between them.
currentLine :: TrailingWhitespace -> Out -> Text
currentLine trailingWhitespace out
  | T.null written = heldBack
  | T.null heldBack = written
  | otherwise = written <> " " <> heldBack
  where
    written =
      adjustTrailingWhitespace
        trailingWhitespace
        (T.concat (reverse (outCurrent out)))
    heldBack =
      maybe
        ""
        (adjustTrailingWhitespace trailingWhitespace)
        (listToMaybe (outHeldBack out))

-- | Handle the given line according to the 'TrailingWhitespace' style.
adjustTrailingWhitespace :: TrailingWhitespace -> Text -> Text
adjustTrailingWhitespace = \case
  TrimWhitespace -> T.stripEnd
  KeepWhitespace -> id

-- | Assemble the final text: one trailing newline, no blank lines at the
-- end, no trailing whitespace anywhere.
finish :: Out -> Text
finish out =
  case dropWhile T.null (outLines (breakLine 0 out)) of
    [] -> ""
    ls -> T.unlines (reverse ls)
