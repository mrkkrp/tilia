{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Extracting comments from a parsed module.
module Tilia.Comments
  ( Comment (..),
    CommentStyle (..),
    Above (..),
    commentsOf,
    transcendentComment,
    renderComment,
    closesItself,
    bracketed,
    commentTrailing,
    singleLine,
    holdsOff,
    continuesHaddock,
    carriedOnFrom,
    carriesOn,
    widenTrigger,
    asOrdinary,
    triggerEscaped,
    opensHaddock,
    opensSectionHeading,
    afterOpener,
    triggerOn,
    commentsWithin,
    Pragma (..),
    commentPragma,
  )
where

import Data.Char (isSpace)
import Data.IntMap.Strict qualified as IntMap
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Parser.Lexer qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import Tilia.Pragma (withoutSkippedComments)
import Tilia.Source.Lines
  ( Lines,
    blankAt,
    columnOf,
    contentColumn,
    directiveOnLine,
    lineAt,
    lineTexts,
    offsetOf,
  )
import Tilia.Span (Span (..), covers)
import Tilia.Span.Ghc (spanOfReal)

-- | One comment.
data Comment = Comment
  { -- | Where it was in the input.
    commentSpan :: Span,
    -- | Its lines, dedented, without trailing whitespace. A line comment
    -- has one; a block comment has one per line it spanned.
    commentBody :: NonEmpty Text,
    -- | How it was written.
    commentStyle :: CommentStyle,
    -- | What was on the line above it.
    commentAbove :: Above,
    -- | Where the code before it on its opening line stops: the column one
    -- past the last character of that code, or 'Nothing' when the comment
    -- had the line to itself.
    commentCodeBeforeStopsAt :: Maybe Int,
    -- | Whether that code ends with an opening bracket.
    commentFirstInBrackets :: Bool,
    -- | Whether anything other than whitespace follows it on its closing
    -- line.
    commentFollowed :: Bool,
    -- | Whether to leave an empty line above it when it is printed.
    commentGapAbove :: Bool,
    -- | Whether to leave an empty line below it when it is printed.
    commentGapBelow :: Bool,
    -- | Where the first line under it that is neither empty nor a directive
    -- begins, if there is one.
    commentNextLine :: Maybe (Int, Int)
  }
  deriving (Eq, Show)

-- | How a comment was written. The distinction is kept because it
-- constrains what may be done with the comment.
data CommentStyle
  = -- | @-- …@
    LineComment
  | -- | @{- … -}@
    BlockComment
  | -- | @-- |@, @-- ^@, @-- *@, @-- $@ and the block forms
    DocComment
  deriving (Eq, Show)

-- | What was on the line above a comment.
data Above
  = -- | Nothing: the comment begins on the first line of the file, or the
    -- line above is not in this configuration.
    TopOfFile
  | -- | An empty line.
    BlankLine
  | -- | Something, beginning at this column.
    ContentAt !Int
  deriving (Eq, Show)

-- | Every comment in a module, in source order.
commentsOf ::
  -- | The module's lines, which every comment is read against.
  Lines ->
  -- | The module's comment tokens, in source order, each where the lexer
  -- found it.
  [(GHC.RealSrcSpan, GHC.Token)] ->
  [Comment]
commentsOf ls = mapMaybe (uncurry (mkComment ls))

-- | Build a comment from a token and the span it occupied, where the token
-- is a comment.
mkComment :: Lines -> GHC.RealSrcSpan -> GHC.Token -> Maybe Comment
mkComment ls spn = \case
  GHC.ITlineComment t _ -> Just (commentAt ls s LineComment (T.pack t))
  GHC.ITblockComment t _ -> Just (commentAt ls s BlockComment (T.pack t))
  GHC.ITdocComment _ _ -> Just (commentAt ls s DocComment (sliceSpan (lineTexts ls) s))
  GHC.ITdocOptions t _ -> Just (commentAt ls s LineComment (T.pack t))
  _ -> Nothing
  where
    s = spanOfReal spn

-- | Build a comment of this style from the span it occupied and its text.
commentAt :: Lines -> Span -> CommentStyle -> Text -> Comment
commentAt ls spn style raw =
  Comment
    { commentSpan = spn,
      commentBody = normalizeBody (spanStartColumn spn) style raw,
      commentStyle = style,
      commentAbove = above,
      commentCodeBeforeStopsAt = codeBeforeStopsAt,
      commentFirstInBrackets = any ((`T.elem` "([{") . snd) (T.unsnoc before'),
      commentFollowed = followed,
      commentGapAbove = above == BlankLine,
      commentGapBelow = blankAt (spanEndLine spn + 1) ls,
      commentNextLine = nextLine
    }
  where
    startColumn = maybe 0 (`offsetOf` spanStartColumn spn) openingLine
    openingLine = lineAt (spanStartLine spn) ls
    lineAbove
      | spanStartLine spn <= 1 = Nothing
      | otherwise = lineAt (spanStartLine spn - 1) ls
    above = case lineAbove of
      Nothing -> TopOfFile
      Just l
        | T.all isSpace l -> BlankLine
        | otherwise -> ContentAt (contentColumn l)
    before' = maybe T.empty (T.stripEnd . T.take startColumn) openingLine
    codeBeforeStopsAt = do
      l <- openingLine
      if T.null before' then Nothing else Just (columnOf l (T.length before'))
    nextLine =
      listToMaybe
        [ (n, contentColumn l)
        | (n, Just l) <-
            takeWhile
              (isJust . snd)
              [(n, lineAt n ls) | n <- [spanEndLine spn + 1 ..]],
          not (T.all isSpace l || isJust (directiveOnLine l))
        ]
    followed = case lineAt (spanEndLine spn) ls of
      Just l -> not (T.all isSpace (T.drop (offsetOf l (spanEndColumn spn)) l))
      Nothing -> False

-- | A comment, with its text read from the module as written rather than
-- from the configuration it was found in.
--
-- The two differ for a block comment with a conditional inside it: the
-- preprocessor does not know what a Haskell comment is, so each
-- configuration has the comment with only its own branch left in.
transcendentComment :: Lines -> Comment -> Comment
transcendentComment ls c = case commentStyle c of
  BlockComment ->
    c
      { commentBody =
          normalizeBody
            (spanStartColumn s)
            BlockComment
            (sliceSpan (lineTexts ls) s)
      }
  _ -> c
  where
    s = commentSpan c

-- | Apply the normalizations, in the only order that works: dedent before
-- stripping, since a line of nothing but spaces has to still count as
-- indented when the common indentation is measured.
normalizeBody :: Int -> CommentStyle -> Text -> NonEmpty Text
normalizeBody column style raw =
  case NE.nonEmpty (T.lines raw) of
    Nothing -> spaceAfterDashes style raw :| []
    Just (first' :| rest) ->
      fmap T.stripEnd (spaceAfterDashes style first' :| fmap dedent rest)
  where
    dedent l =
      let n = offsetOf (T.takeWhile isSpace l) column
       in T.replicate (columnOf l n - column) " " <> T.drop n l

-- | @--foo@ becomes @-- foo@; @----@ and @-- foo@ are left alone.
--
-- Only the opening line of a line comment is eligible. Inside a block
-- comment a @--@ is just two characters the author wrote.
spaceAfterDashes :: CommentStyle -> Text -> Text
spaceAfterDashes BlockComment t = t
spaceAfterDashes _ t = case T.stripPrefix "--" t of
  Nothing -> t
  Just rest -> case T.uncons rest of
    Nothing -> t
    Just (c, _)
      | c == ' ' || c == '-' -> t
      | otherwise -> "-- " <> rest

-- | The text a span covers.
sliceSpan :: [Text] -> Span -> Text
sliceSpan sourceLines spn =
  T.intercalate "\n" (zipWith clip [startLine ..] covered)
  where
    covered =
      take (endLine - startLine + 1) (drop (startLine - 1) sourceLines)
    clip n l =
      (if n == startLine then T.drop (offsetOf l startCol) else id)
        . (if n == endLine then T.take (offsetOf l endCol) else id)
        $ l
    startLine = spanStartLine spn
    endLine = spanEndLine spn
    startCol = spanStartColumn spn
    endCol = spanEndColumn spn

-- | Put a comment back together as it will appear in the output.
renderComment :: Comment -> Text
renderComment = T.intercalate "\n" . NE.toList . commentBody

-- | Does this comment let code follow it on the same line?
closesItself :: Comment -> Bool
closesItself c = commentStyle c == BlockComment && singleLine c

-- | Was this comment written between brackets rather than as @--@ lines?
bracketed :: Comment -> Bool
bracketed c = "{-" `T.isPrefixOf` T.stripStart (NE.head (commentBody c))

-- | Was the comment written after code on its line?
commentTrailing :: Comment -> Bool
commentTrailing = isJust . commentCodeBeforeStopsAt

-- | Is this a Haddock written as @--@ lines that can go on past its first
-- line, so that a comment written against it is held off from it?
holdsOff :: Comment -> Bool
holdsOff c =
  commentStyle c == DocComment
    && not (bracketed c)
    && not (opensSectionHeading (NE.head (commentBody c)))

-- | Would the lexer read this comment as more of a Haddock written as @--@
-- lines right above it?
continuesHaddock :: Comment -> Bool
continuesHaddock c =
  not (bracketed c) && not ("---" `T.isPrefixOf` NE.head (commentBody c))

-- | Is this comment a single line?
singleLine :: Comment -> Bool
singleLine c = case commentBody c of
  (_ :| []) -> True
  _ -> False

-- | Does a comment under a remark carry it on, rather than go with the code
-- after it: is it no further left than the remark, or does that code begin
-- further left than the comment?
carriesOn ::
  -- | The column the remark begins at.
  Int ->
  -- | The comment.
  Span ->
  -- | Where the code after it begins, if any follows.
  Maybe Span ->
  Bool
carriesOn remark comment after =
  remark <= spanStartColumn comment
    || all (\r -> spanStartColumn r < spanStartColumn comment) after

-- | The line whose trailing comment a comment with the line to itself
-- carries on, by being lined up under that line or that comment, or under
-- comments that carry it on in turn, and the column that comment begins at.
-- A trailing comment that closes itself is carried on by none.
carriedOnFrom :: [Comment] -> Comment -> Maybe (Int, Int)
carriedOnFrom cs = \c ->
  if commentTrailing c || commentFollowed c
    then Nothing
    else
      go
        (spanStartColumn (commentSpan c))
        (spanStartLine (commentSpan c) - 1)
        (commentAbove c)
  where
    remarks =
      IntMap.fromList
        [ (spanEndLine s, spanStartColumn s)
        | c <- cs,
          commentTrailing c,
          not (commentFollowed c),
          not (closesItself c),
          let s = commentSpan c
        ]
    ownLineComments =
      IntMap.fromList
        [ (spanStartLine s, (spanStartColumn s, commentAbove c))
        | c <- cs,
          not (commentTrailing c),
          not (commentFollowed c),
          let s = commentSpan c
        ]
    go column line above
      | Just remark <- IntMap.lookup line remarks,
        above == ContentAt column || remark <= column =
          Just (line, remark)
      | Just (col, above') <- IntMap.lookup line ownLineComments,
        col == column,
        above == ContentAt column =
          go column (line - 1) above'
      | otherwise = Nothing

-- | Put a space between a doc comment's trigger and the text after it, so
-- that @-- |Foo@ comes out as @-- | Foo@.
--
-- Only doc comments have triggers; on anything else this is a no-op.
widenTrigger :: Comment -> Comment
widenTrigger c
  | DocComment <- commentStyle c,
    (headLine :| rest) <- commentBody c,
    Just (upToTrigger, body) <- splitTrigger headLine,
    not (T.null body),
    not (" " `T.isPrefixOf` body) =
      c{commentBody = (upToTrigger <> " " <> body) :| fmap shiftOne rest}
  | otherwise = c
  where
    shiftOne l = case openerWidth l of
      Just _ -> l
      Nothing -> " " <> l

-- | Put a backslash in front of a doc comment's trigger.
--
-- For a doc comment the compiler did not manage to attach to anything: it
-- is going to come back out as an ordinary comment, and written as it
-- stands it would be lexed as a doc comment again on the next pass, so the
-- formatter would not have a fixed point. The backslash is what Haddock
-- reads as \"this is not a trigger\".
escapeTrigger :: Comment -> Comment
escapeTrigger c = case commentStyle c of
  DocComment ->
    c
      { commentBody = case commentBody c of
          opening :| rest -> escape opening :| rest,
        commentStyle = if bracketed c then BlockComment else LineComment
      }
  _ -> c
  where
    escape l = case openerWidth l of
      Just n
        | (gap, rest) <- T.span (== ' ') (T.drop n l),
          triggered rest ->
            T.take n l <> (if T.null gap then " " else gap) <> "\\" <> rest
      _ -> l

-- | A doc comment the compiler did not attach to anything, as the ordinary
-- comments the next pass reads once its trigger is escaped.
--
-- Nothing joins the lines of a @--@ one any more, so each line is a comment
-- of its own, placed on its own account.
asOrdinary :: Lines -> Comment -> [Comment]
asOrdinary ls c
  | commentStyle c == DocComment,
    not (bracketed c),
    not (singleLine c) =
      fmap escapeTrigger (mapMaybe lineOf [spanStartLine s .. spanEndLine s])
  | otherwise = [escapeTrigger c]
  where
    s = commentSpan c
    lineOf n = do
      l <- lineAt n ls
      let piece =
            Span
              { spanStartLine = n,
                spanStartColumn =
                  if n == spanStartLine s
                    then spanStartColumn s
                    else contentColumn l,
                spanEndLine = n,
                spanEndColumn =
                  if n == spanEndLine s
                    then spanEndColumn s
                    else columnOf l (T.length l)
              }
      pure (commentAt ls piece DocComment (sliceSpan (lineTexts ls) piece))

-- | Has 'asOrdinary' escaped this comment's trigger?
--
-- What it was written as cannot be read off the comment any more—that is
-- the point of escaping—so anything wanting to know whether a comment
-- started life as a Haddock has to ask this.
triggerEscaped :: Comment -> Bool
triggerEscaped c = case T.uncons =<< afterOpener (NE.head (commentBody c)) of
  Just ('\\', rest) -> triggered rest
  _ -> False

-- | Does this text begin with one of the characters that opens a Haddock?
triggered :: Text -> Bool
triggered t = case T.uncons t of
  Just (ch, _) -> ch `elem` ("|^*$" :: String)
  Nothing -> False

-- | Does this line open a Haddock?
opensHaddock :: Text -> Bool
opensHaddock = isJust . splitTrigger

-- | Does this line open a Haddock section heading, @-- *@?
opensSectionHeading :: Text -> Bool
opensSectionHeading = maybe False (T.isSuffixOf "*" . fst) . splitTrigger

-- | Split a doc comment's opening line into everything up to and including
-- its trigger, and whatever follows.
splitTrigger :: Text -> Maybe (Text, Text)
splitTrigger l = do
  n <- openerWidth l
  let (opener, rest) = T.splitAt n l
      (gap, inside) = T.span (== ' ') rest
  trigger <- triggerOn inside
  pure (opener <> gap <> trigger, T.drop (T.length trigger) inside)

-- | What follows the @--@ or @{-@ that opens a comment, and the spaces after
-- it, if the line opens one.
afterOpener :: Text -> Maybe Text
afterOpener l = (\n -> T.dropWhile (== ' ') (T.drop n l)) <$> openerWidth l
{-# INLINE afterOpener #-}

-- | How many characters open a comment, if it opens one.
openerWidth :: Text -> Maybe Int
openerWidth l
  | "--" `T.isPrefixOf` l = Just 2
  | "{-" `T.isPrefixOf` l = Just 2
  | otherwise = Nothing

-- | The trigger at the start of what follows a comment's opener: @|@, @^@,
-- or a run of @*@.
triggerOn :: Text -> Maybe Text
triggerOn inside = case T.uncons inside of
  Just ('|', _) -> Just "|"
  Just ('^', _) -> Just "^"
  Just ('*', _) -> Just (T.takeWhile (== '*') inside)
  _ -> Nothing

-- | The comments written inside a region.
commentsWithin :: Span -> [Comment] -> [Comment]
commentsWithin s = filter (covers s . commentSpan)

-- | A compiler pragma, which is written as a block comment but is not one.
--
-- GHC reads pragmas only from the file header, so where a pragma sits
-- decides whether it does anything at all. That is why recognising one is
-- not enough on its own: see 'Tilia.Parser.pmHeaderEnd' for the boundary
-- that says which pragmas are real.
data Pragma = Pragma
  { -- | The name, upper-cased as GHC expects it, e.g. @LANGUAGE@.
    pragmaName :: Text,
    -- | Everything between the name and the closing @#-}@, with the
    -- surrounding whitespace removed, on one line, and without the comments
    -- GHC skips in a @LANGUAGE@ pragma.
    pragmaBody :: Text,
    -- | Whether it holds such a comment.
    pragmaCommented :: Bool
  }
  deriving (Eq, Show)

-- | Recognise a pragma.
commentPragma :: Comment -> Maybe Pragma
commentPragma c = do
  inner <- T.stripSuffix "#-}" =<< T.stripPrefix "{-#" (T.strip written)
  let uncommented = withoutSkippedComments inner
      (name, body) = T.break isSpace (T.stripStart uncommented)
  if T.null name
    then Nothing
    else
      Just
        Pragma
          { pragmaName = T.toUpper name,
            pragmaBody = T.strip (T.unwords (fmap T.strip (T.lines body))),
            pragmaCommented = uncommented /= inner
          }
  where
    written = T.intercalate "\n" (NE.toList (commentBody c))
