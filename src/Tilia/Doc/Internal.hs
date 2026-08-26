{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The document representation and the engine that turns it into text.
--
-- Printing code should not import this module; import
-- "Tilia.Doc.Combinators" instead, which exposes 'Doc' abstractly along
-- with the vocabulary for building one. What needs the constructors is the
-- engine below, the pass that walks a finished document to put the comments
-- into it ("Tilia.Comments.Attach"), and tests that look inside a document.
-- Those three take a document apart rather than build one, which is the
-- thing the vocabulary cannot express.
--
-- The printer is split in two halves that meet at 'Doc'. Code that walks
-- the syntax tree builds a 'Doc', which is an ordinary immutable value with
-- no notion of columns, indentation or what has already been written. The
-- engine in this module is the only thing that knows about those, and it
-- learns them by walking the finished document. Nothing in the first half
-- can observe the second, which is what keeps printing code from having to
-- reason about emission order.
module Tilia.Doc.Internal
  ( -- * Documents
    Doc (..),
    Layout (..),
    Resume (..),
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
    --
    -- Two in a row leave one empty line between what surrounds them, which
    -- is all a blank line is; there is deliberately no separate constructor
    -- for one. Further breaks add nothing.
    DHardBreak
  | -- | Text to be put at the end of the line this position falls on,
    -- however much of the line is still to be written.
    --
    -- For a comment the author wrote at the end of a line. Where it belongs
    -- is not a position in the document but a position in the /output/: it
    -- has to come after everything else on its line, including punctuation
    -- the printer has not emitted yet. Putting it in the document where the
    -- node it trails happens to sit would push a comma, an arrow or a
    -- closing bracket onto the next line.
    --
    -- One line holds one of these at its end. A second means two comments
    -- trailing what turned out to be a single line of output, and it goes on
    -- a line of its own underneath rather than being run together with the
    -- first into a comment neither author wrote.
    DHoldBack !Text
  | -- | Close the line, and let a break that immediately follows know that
    -- it has nothing left to do.
    --
    -- A comment owns the rest of its line, so something has to end that
    -- line; but whatever the comment was attached to very often ends it
    -- too, and two breaks in a row are a blank line. This is the break that
    -- says \"the line is finished\" rather than \"break here\", so the two
    -- do not add up to an empty line nobody asked for.
    DCloseLine
  | -- | A line break between two lines of text that is being reproduced
    -- rather than laid out.
    --
    -- Collapsing nothing and skipping nothing, unlike every other break
    -- here: the lines either side are the author's, so an empty one among
    -- them is content and not spacing. Where the next line begins is the
    -- only thing left to decide, and 'Resume' decides it.
    DVerbatimBreak !Resume
  | -- | Concatenation. See the 'Semigroup' instance.
    DCat !Doc !Doc
  | -- | Indent the enclosed document by the given number of steps, relative
    -- to the current indentation.
    DNest !Int !Doc
  | -- | Indent the enclosed document to whatever column the line has
    -- reached, so that it lines up under itself when broken.
    DAlign !Doc
  | -- | Lay the enclosed document out flat or broken.
    --
    -- The decision is already made by the time it reaches the engine.
    -- 'groupLayout' is what makes it, from the span the construct occupied
    -- in the input, and it lives in the combinator layer's vocabulary
    -- rather than here so that the engine has no policy in it at all.
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
    -- This carries no layout meaning at all and the engine ignores it.
    -- Keeping provenance separate from grouping is deliberate: the two
    -- coincide often, but a construct can need one without the other, and
    -- fusing them is what forces a printer to grow an escape hatch for each
    -- case where they come apart.
    DLocated !Span !Doc
  | -- | Fence prevents comments inside from floating out and attaching to
    -- elements they are not supposed to attach to.
    DFence !Span !Doc
  | -- | Alternatives the preprocessor chooses between, and the condition it
    -- chooses on.
    DCppChoice ![(Text, Doc)] !Doc
  | -- | A preprocessor line that is not a conditional, reproduced.
    DCppDirective !Text
  deriving (Eq, Show)

-- | Documents concatenate. @'DEmpty'@ is the unit, so a document is a
-- monoid and printing code can use @'mconcat'@, @'foldMap'@ and the rest of
-- the ordinary vocabulary instead of a bespoke sequencing operator.
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
data Resume
  = -- | At the indentation in force, as any other break would.
    AtIndent
  | -- | At column zero, whatever the indentation.
    AtMargin
  deriving (Eq, Show)

-- | Decide how to lay a group out.
--
-- This is the whole of the policy, in one place on purpose. Layout follows
-- the input: a construct written on one line stays on one line, and one
-- that was spread out stays spread out. A group with no span is one the
-- printer synthesised rather than read, and has nothing to follow, so it
-- goes flat.
--
-- Notably absent is any notion of a maximum line width. Nothing in the
-- engine measures the result against a limit, so a long line that was
-- written as one line is reproduced as one line.
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
defaultRenderOptions = RenderOptions {roIndentStep = 2}

-- | What the engine carries while walking a document.
--
-- Indentation and layout flow downwards and are restored on the way out, so
-- they are passed as arguments. Everything else is output being
-- accumulated.
data Env = Env
  { envIndent :: !Int,
    envLayout :: !Layout,
    envIndentStep :: !Int
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
    -- of their own under it, since two comments run together would be one
    -- comment neither author wrote.
    outHeldBack :: ![Text],
    -- | Whether the line was closed by something that already knew it was
    -- ending it, so that a break arriving now would add an empty line rather
    -- than end anything.
    outClosed :: !Bool
  }

emptyOut :: Out
emptyOut =
  Out
    { outLines = [],
      outCurrent = [],
      outColumn = 0,
      outStarted = False,
      outHeldBack = [],
      outClosed = False
    }

-- | Turn a document into text.
render :: RenderOptions -> Doc -> Text
render opts doc = finish (go env doc emptyOut)
  where
    env =
      Env
        { envIndent = 0,
          envLayout = Broken,
          envIndentStep = roIndentStep opts
        }

-- | Walk a document, accumulating output.
go :: Env -> Doc -> Out -> Out
go env = \case
  DEmpty -> id
  DText t -> putText (envIndent env) t
  DSpace -> putSpace
  DBreak -> case envLayout env of
    Flat -> putSpace
    Broken -> breakLine (envIndent env)
  DSoftBreak -> case envLayout env of
    Flat -> id
    Broken -> breakLine (envIndent env)
  DHoldBack t -> putHeldBack (envIndent env) t
  DCloseLine -> closeLine (envIndent env)
  DHardBreak -> breakLine (envIndent env)
  DVerbatimBreak resume -> verbatimBreakLine resume
  DCat a b -> go env b . go env a
  DNest n d -> go env {envIndent = envIndent env + n * envIndentStep env} d
  DAlign d -> \out ->
    go env {envIndent = max (envIndent env) (outColumn out)} d out
  DGroup l d -> go env {envLayout = l} d
  DVariant flatD brokenD -> case envLayout env of
    Flat -> go env flatD
    Broken -> go env brokenD
  DLocated _ d -> go env d
  DFence _ d -> go env d
  DCppChoice branches fallback ->
    foldr (flip (.)) id . concat $
      [ [atMargin ("#" <> guard'), go env taken]
        | (guard', taken) <- branches
      ]
        <> [[atMargin "#else", go env fallback] | fallback /= DEmpty]
        <> [[atMargin "#endif"]]
  DCppDirective t -> atMargin ("#" <> t)

-- | Put a line of text at the margin, on a line of its own.
--
-- A preprocessor directive is not part of the program's layout and does not
-- take its indentation: it begins where the line begins, whatever is in
-- force around it. The line before it is closed only if anything was
-- written to it, so a directive following something that already ended its
-- line does not leave an empty one behind.
atMargin :: Text -> Out -> Out
atMargin t = closeLine 0 . putText 0 t . closeLine 0

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
    out = out0 {outClosed = False}

-- | Hold a fragment back until the line ends.
putHeldBack :: Int -> Text -> Out -> Out
putHeldBack indent t out
  | outStarted out || not (null (outHeldBack out)) =
      out {outHeldBack = outHeldBack out <> [t], outClosed = False}
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

endsWithSpace :: Out -> Bool
endsWithSpace out = case outCurrent out of
  (t : _) -> maybe False ((== ' ') . snd) (T.unsnoc t)
  [] -> False

-- | Close the current line, if there is anything on it.
--
-- Unlike 'breakLine' this leaves a mark: the next break sees that the line
-- was already ended on purpose and does nothing, so a comment that ends its
-- own line and a construct that would have ended it anyway do not between
-- them leave an empty one.
closeLine ::
  -- | Where the line after this one begins
  Int ->
  Out ->
  Out
closeLine indent out
  | hasContent out = (breakLine indent out) {outClosed = True}
  | otherwise = out

-- | Finish the current line.
--
-- Two breaks in a row leave one empty line between the text either side of
-- them, which is a blank line the author asked for. Further breaks add
-- nothing: the output never carries two blank lines in a row, however many
-- times printing code breaks. Breaking before anything has been written is
-- dropped for the same reason, since 'finish' strips empty lines only from
-- the end. Nor is an empty line written at the top of a block, where there
-- is nothing above it to be held off.
--
-- Between them these rules mean printing code may break wherever a break
-- might be wanted without first working out what it already emitted.
breakLine ::
  -- | Where the line after this one begins
  Int ->
  Out ->
  Out
breakLine indent out
  | outClosed out = out {outClosed = False}
  | atStart out = out
  | not (hasContent out), repeatsBlank out || opensABlock indent out = discarded
  | otherwise = discarded {outLines = overflow indent out <> outLines out}
  where
    discarded =
      out {outCurrent = [], outColumn = 0, outStarted = False, outHeldBack = []}

-- | Every line the break that has just happened produces.
--
-- Usually one: the line that was being built. There are more when several
-- fragments were held back for it, because only the first of them can go at
-- its end and the rest need lines of their own. Held-back fragments are
-- always comments, so several mean several comments that trailed different
-- things in the input which have turned out to share a line of output; run
-- together they would read as one comment nobody wrote, so each of the
-- others gets a line below.
--
-- Held back until here rather than written when it arrived, because the
-- line was not finished then. A comma or a closing bracket still to come
-- would have been pushed underneath the comment.
--
-- Ordered as 'outLines' is, most recent first, ready to be put in front
-- of it.
overflow ::
  -- | Where the line after these would begin, used only if there is no line
  -- to take the indentation from
  Int ->
  Out ->
  [Text]
overflow indent out = reverse (finished : map below spilled)
  where
    finished = currentLine out
    spilled = drop 1 (outHeldBack out)

    -- Indented to match the line they spilled from rather than to the
    -- indentation in force, so that they stay under the thing they were
    -- written against instead of under whatever encloses it.
    below t = T.replicate column " " <> T.stripEnd t
    column
      | T.null finished = indent
      | otherwise = T.length (T.takeWhile (== ' ') finished)

-- | Would an empty line here be the first thing inside a block?
opensABlock :: Int -> Out -> Bool
opensABlock indent out = case outLines out of
  (l : _) -> T.length l <= indent
  [] -> False

-- | Finish the current line between two lines of reproduced text.
verbatimBreakLine :: Resume -> Out -> Out
verbatimBreakLine resume out =
  out
    { outLines = overflow 0 out <> outLines out,
      outCurrent = [],
      outColumn = 0,
      outStarted = resume == AtMargin,
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
-- for its end, with one space between them and no trailing whitespace.
currentLine :: Out -> Text
currentLine out
  | T.null written = heldBack
  | T.null heldBack = written
  | otherwise = written <> " " <> heldBack
  where
    written = T.stripEnd (T.concat (reverse (outCurrent out)))
    heldBack = maybe "" T.stripEnd (listToMaybe (outHeldBack out))

-- | Assemble the final text: one trailing newline, no blank lines at the
-- end, no trailing whitespace anywhere.
finish :: Out -> Text
finish out =
  case dropWhile T.null (outLines (breakLine 0 out)) of
    [] -> ""
    ls -> T.unlines (reverse ls)
