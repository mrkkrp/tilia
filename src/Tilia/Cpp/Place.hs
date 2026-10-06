{-# LANGUAGE LambdaCase #-}

-- | Placing elements that the configurations of a module do not print (CPP
-- conditionals, directives, and comments).
module Tilia.Cpp.Place
  ( CommentSummary,
    summarizeComments,
    restoreUnprinted,
    regionOf,
  )
where

import Control.Monad (foldM)
import Data.List (sortOn, unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Monoid (Any (..))
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Tilia.Comments
  ( Comment (..),
    bracketed,
    commentPragma,
    holdsOff,
    transcendentComment,
  )
import Tilia.Comments.Attach (Margin (..), attachScopedComments)
import Tilia.Cpp.Directives
  ( CppError (..),
    Directive (..),
    GroupSpec (..),
    Guard (..),
    allGroups,
    dSpan,
    isDirective,
    macroLines,
    opaqueDirectives,
    readConditionals,
  )
import Tilia.Doc.Combinators (blankLine, hardBreak, includeWhen, indent)
import Tilia.Doc.Internal
  ( Conditional (..),
    Doc (..),
    conditionalRange,
    foldChildren,
    mapChildren,
    onlySpacing,
    printsNothing,
    spine,
    unwrap,
    wrap,
  )
import Tilia.Source (Lines, Written (..), blankAt, lineAt, linesOf)
import Tilia.Span (Span (..), mkSpan)

-- | What the configurations of a module had besides their code.
data CommentSummary = CommentSummary
  { -- | The comments no syntax tree carries, by where they were written.
    summaryLoose :: Map Span Comment,
    -- | Where every comment was written, whether a tree carries it or not.
    summaryComments :: Set Span,
    -- | Where every Haddock that holds off a comment was written.
    summaryHaddocks :: Set Span
  }

-- | What two sets of configurations found between them.
--
-- A comment is the same comment in every configuration it is in, except
-- for whether it wants an empty line either side of it, which is asked of
-- what it is written next to and so can come out differently. It gets one
-- wherever any configuration wanted one.
instance Semigroup CommentSummary where
  a <> b =
    CommentSummary
      { summaryLoose = Map.unionWith either' (summaryLoose a) (summaryLoose b),
        summaryComments = Set.union (summaryComments a) (summaryComments b),
        summaryHaddocks = Set.union (summaryHaddocks a) (summaryHaddocks b)
      }
    where
      either' x y =
        x
          { commentGapAbove = commentGapAbove x || commentGapAbove y,
            commentGapBelow = commentGapBelow x || commentGapBelow y
          }

instance Monoid CommentSummary where
  mempty = CommentSummary Map.empty Set.empty Set.empty

-- | What one configuration found.
summarizeComments ::
  -- | The comments its syntax tree does not carry.
  [Comment] ->
  -- | Every comment in it.
  [Comment] ->
  CommentSummary
summarizeComments loose every =
  CommentSummary
    { summaryLoose = Map.fromList [(commentSpan c, c) | c <- loose],
      summaryComments = Set.fromList (fmap commentSpan every),
      summaryHaddocks = Set.fromList [commentSpan c | c <- every, holdsOff c]
    }

-- | Place comments into a document is the result of merging different
-- configurations.
restoreUnprinted ::
  -- | The module as written.
  Text ->
  -- | What its configurations found.
  CommentSummary ->
  -- | The document its configurations were merged into.
  Doc ->
  Either CppError Doc
restoreUnprinted source found doc = do
  forest <- readConditionals source
  let groups = [gs | gs <- allGroups forest, not (inComment (fst (gsWhole gs)))]
      scope = scopeIn (fmap gsConditional groups)
      notes =
        [ if any directiveAt [spanStartLine s + 1 .. spanEndLine s - 1]
            then transcendentComment written c
            else c
        | c <- Map.elems (summaryLoose found),
          let s = commentSpan c
        ]
  let shelled = foldl (restoreConditional written) (widened doc) groups
      marked = spacedApart written (realized (widened shelled))
      printed = Set.fromList (Nothing : fmap scope (spansIn marked))
      noted =
        attachScopedComments
          margin
          scope
          (filter ((`Set.member` printed) . scope . commentSpan) notes)
          marked
      margin =
        Margin
          { marginLine = isDirective,
            marginComment = isNothing . commentPragma
          }
  directed <-
    foldM
      (putDirective written)
      noted
      (filter (not . inComment . dLine) (opaqueDirectives source))
  placed <-
    foldM
      (putMacroLine written)
      directed
      (filter (not . inComment . fst) macros)
  pure $
    keptApart
      (summaryHaddocks found)
      (Set.fromList [commentSpan c | c <- notes, not (bracketed c)])
      placed
  where
    written = linesOf (Written source)
    macros = macroLines source
    directiveAt n =
      maybe False isDirective (lineAt n written) || any ((== n) . fst) macros
    inComment n =
      any
        (\s -> spanStartLine s < n && n < spanEndLine s)
        (summaryComments found)

-- | What was printed last, as far as two comments running into each other
-- goes.
data Printed
  = -- | Something that keeps them apart.
    Apart
  | -- | A Haddock written as @--@ lines.
    AHaddock
  | -- | A comment written as @--@ lines.
    ANote
  deriving (Eq)

-- | Put an empty line between a comment written as @--@ lines and a Haddock
-- written the same way wherever one comes out right under the other: under
-- a Haddock a comment would be read as more of it, and above one it is kept
-- apart from it.
keptApart ::
  -- | Where the Haddocks written as @--@ lines are.
  Set Span ->
  -- | Where the comments written as @--@ lines are.
  Set Span ->
  Doc ->
  Doc
keptApart haddocks notes = snd . go Apart
  where
    go before = \case
      DLocated s x
        | Set.member s notes ->
            (ANote, DLocated s (includeWhen (before == AHaddock) blankLine <> x))
        | Set.member s haddocks ->
            (AHaddock, DLocated s (includeWhen (before == ANote) blankLine <> snd (go Apart x)))
      DCat a b ->
        let (between, a') = go before a
            (after, b') = go between b
         in (after, DCat a' b')
      DVariant a b -> DVariant (snd (go before a)) <$> go before b
      d@DCppChoice{} -> (Apart, mapChildren (snd . go Apart) d)
      d
        | Just (w, x) <- unwrap d -> wrap w <$> go before x
        | onlySpacing d || printsNothing d -> (before, d)
        | otherwise -> (Apart, d)

-- | Widen every region to take in the conditionals printed inside it.
widened :: Doc -> Doc
widened = fst . go
  where
    go = \case
      DLocated s x -> let (x', c) = go x in (DLocated (maybe s (s <>) c) x', c)
      DFence s x -> let (x', c) = go x in (DFence (maybe s (s <>) c) x', c)
      DCppChoice cs bs e ->
        let bs' = [(g, go d) | (g, d) <- bs]
            (e', c) = go e
         in ( DCppChoice cs [(g, d) | (g, (d, _)) <- bs'] e',
              foldMap conditionalSpan cs <> foldMap (snd . snd) bs' <> c
            )
      DCat a b ->
        let (a', ca) = go a
            (b', cb) = go b
         in (DCat a' b', ca <> cb)
      DVariant a b -> let (b', c) = go b in (DVariant (fst (go a)) b', c)
      d
        | Just (w, x) <- unwrap d -> let (x', c) = go x in (wrap w x', c)
        | otherwise -> (d, Nothing)

-- | The lines a conditional was written on, from its @#if@ to its @#endif@.
conditionalSpan :: Conditional -> Maybe Span
conditionalSpan c = (\(from, to) -> mkSpan (from, 1) (to, 1)) <$> conditionalRange c

-- | The part of the module a region was written in: the innermost branch of
-- a conditional holding all of it, or 'Nothing' for the module itself.
--
-- A region is printed in exactly the configurations that take its branch,
-- so this is also which configurations it is in, and a comment written in
-- one branch goes only next to what that branch holds.
scopeIn :: [Conditional] -> Span -> Maybe (Int, Int)
scopeIn conditionals s =
  listToMaybe
    ( sortOn
        (Down . fst)
        [ (from, to)
        | ls <- fmap conditionalLines conditionals,
          (from, to) <- zip ls (drop 1 ls),
          from < spanStartLine s,
          spanEndLine s < to
        ]
    )

-- | Mark out every choice the merge made as the region of the input it was
-- printed from, and give every branch of it something to hold on to.
--
-- A choice's region is its conditionals, from the first @#if@ to the last
-- @#endif@, and anything else its alternatives print: a comment written
-- above the @#if@ goes before the whole choice, not into one alternative of
-- it. Every alternative also gets an anchor where its conditional opens, for
-- a comment written above the @#if@ when the alternatives print code from
-- above it too, and one where each branch closes, for a comment written in a
-- branch with nothing after it in that branch.
realized :: Doc -> Doc
realized = \case
  DCppChoice cs bs e ->
    let alternatives = unified (fmap realized (fmap snd bs <> [e]))
        bs' =
          [ (g, anchoredIn cs k d)
          | (k, (g, d)) <- zip [0 ..] (zip (fmap fst bs) alternatives)
          ]
        e' = anchoredIn cs (length bs) (maybe mempty snd (unsnoc alternatives))
        inner = DCppChoice cs bs' e'
     in maybe
          inner
          ((`DLocated` inner) . pastTheEnd)
          (foldMap conditionalSpan cs <> regionOf inner)
  d -> mapChildren realized d

-- | A span taken past the end of its last line.
pastTheEnd :: Span -> Span
pastTheEnd s = s{spanEndColumn = farRight}

-- | Space two conditionals that come one right after the other as the
-- author spaced them.
spacedApart :: Lines -> Doc -> Doc
spacedApart written = go
  where
    go = \case
      d@(DCat _ _) -> mconcat (apart (fmap go (spine d)))
      d -> mapChildren go d
    apart = \case
      x : rest
        | Just (_, to) <- ownLines x,
          (_ : _, y : rest') <- span onlySpacing rest,
          Just _ <- ownLines y ->
            x : (if blankAt (to + 1) written then blankLine else hardBreak) : apart (y : rest')
      x : rest -> x : apart rest
      [] -> []
    ownLines = \case
      DLocated s (DCppChoice cs _ _)
        | Just c <- foldMap conditionalSpan cs,
          spanStartLine c == spanStartLine s,
          spanEndLine c == spanEndLine s ->
            Just (spanStartLine s, spanEndLine s)
      _ -> Nothing

-- | Give the alternatives of a choice one span for every construct that
-- begins at the same place in more than one of them.
--
-- A choice that prints code written outside its conditionals prints it in
-- every alternative, and a construct with a conditional inside it can end
-- in a different place in each. It is one construct all the same, and a
-- comment written against it has to find it in every one of them.
unified :: [Doc] -> [Doc]
unified ds = fmap (rewrite Set.empty) ds
  where
    hulls =
      Map.filter
        ((> 1) . length)
        (Map.fromListWith (<>) [(startPoint' s, [s]) | d <- ds, s <- outermost d])
    rewrite seen = \case
      DLocated s x
        | Set.notMember (startPoint' s) seen,
          Just ss <- Map.lookup (startPoint' s) hulls ->
            DLocated (foldr (<>) s ss) (rewrite (Set.insert (startPoint' s) seen) x)
        | otherwise -> DLocated s (rewrite (Set.insert (startPoint' s) seen) x)
      d@DCppChoice{} -> d
      d -> mapChildren (rewrite seen) d
    outermost = go Set.empty
      where
        go seen = \case
          DLocated s x
            | Set.member (startPoint' s) seen -> go seen x
            | otherwise -> s : go (Set.insert (startPoint' s) seen) x
          DCppChoice{} -> []
          d -> foldChildren (go seen) d
    startPoint' s = (spanStartLine s, spanStartColumn s)

-- | Give one alternative of a choice its anchors.
anchoredIn ::
  -- | The conditionals the choice was printed from.
  [Conditional] ->
  -- | Which alternative this is.
  Int ->
  Doc ->
  Doc
anchoredIn cs k d = foldl' anchor d (filter (here . fst) (concatMap anchors cs))
  where
    anchor x (n, s) = placeAt (const False) Nothing n (DLocated s mempty) id x
    here n = case alternativeAt n cs of
      Everywhere -> True
      Only i -> i == k
      Nowhere -> False
    anchors c = case conditionalLines c of
      ls@(opening : _) ->
        (opening, mkSpan (opening, 1) (opening, 2))
          : [ (closing, mkSpan (closing - 1, farRight) (closing - 1, farRight))
            | (from, closing) <- take 1 (drop k (zip ls (drop 1 ls))),
              closing - 1 > from
            ]
      [] -> []

-- | A column past the end of any line, for an anchor that has to come after
-- everything written on its line.
farRight :: Int
farRight = maxBound `div` 2

-- | The span of every region a document records.
spansIn :: Doc -> [Span]
spansIn = \case
  DLocated s d -> s : spansIn d
  DFence s d -> s : spansIn d
  d -> foldChildren spansIn d

-- | The smallest span covering every region a document records.
regionOf :: Doc -> Maybe Span
regionOf = \case
  DLocated s _ -> Just s
  DFence s _ -> Just s
  d -> foldChildren regionOf d

-- | Make sure a conditional is in the document, putting it where it was
-- written if the merge left it out.
restoreConditional :: Lines -> Doc -> GroupSpec -> Doc
restoreConditional written doc gs =
  placeAt realized' (Just written) opening shell id doc
  where
    realized' ctx = any (all (`elem` ctx)) (realizations c doc)
    c = gsConditional gs
    (opening, closing) = gsWhole gs
    guards = fmap guardText (gsGuards gs)
    shell =
      DCppChoice [c] [(g, mempty) | g <- guards] mempty
        <> includeWhen (blankAt (closing + 1) written) blankLine

-- | Put a directive back where it was written.
putDirective :: Lines -> Doc -> Directive -> Either CppError Doc
putDirective written doc d
  | quotedAt doc (dLine d) = Left (DirectiveInQuotedText (dLine d) (dKeyword d))
  | otherwise = Right (placeAt (const False) (Just written) (dLine d) body id doc)
  where
    body =
      DCppDirective (dSpan d) (dText d)
        <> includeWhen (any (`blankAt` written) [dLastLine d, dLastLine d + 1]) blankLine

-- | Put a line using a macro back where it was written, at the indentation
-- of the code around it, since what it stands for is code.
putMacroLine :: Lines -> Doc -> (Int, Text) -> Either CppError Doc
putMacroLine written doc (n, t)
  | quotedAt doc n = Left (MacroInQuotedText n t)
  | otherwise = Right (placeAt (const False) (Just written) n body indent doc)
  where
    body =
      DLocated (mkSpan (n, 1) (n, 1)) (DCloseLine <> DText t <> DCloseLine)
        <> includeWhen (blankAt (n + 1) written) blankLine

-- | Is this line inside something the document reproduces verbatim, such
-- as a quasi-quotation, where a line cannot be put back?
quotedAt :: Doc -> Int -> Bool
quotedAt doc n = any inside (located doc)
  where
    inside (s, x) = spanStartLine s < n && n <= spanEndLine s && reproduced x

    located = \case
      DLocated s x -> (s, x) : located x
      DFence s x -> (s, x) : located x
      d -> foldChildren located d

    reproduced = \case
      DVerbatimBreak _ _ -> True
      DLocated{} -> False
      DFence{} -> False
      d -> getAny (foldChildren (Any . reproduced) d)

-- | Put a document at a line of the input, in every alternative that line
-- is printed in.
placeAt ::
  -- | Whether what is being put there is there already, for the
  -- configurations that take these alternatives.
  (Context -> Bool) ->
  -- | The module as written, to ask whether the author left an empty line
  -- above the line; without it the document goes after whatever space is
  -- there.
  Maybe Lines ->
  -- | The line.
  Int ->
  -- | What to put there.
  Doc ->
  -- | How to put it among the lines of something that begins above it, at
  -- that thing's own indentation.
  (Doc -> Doc) ->
  Doc ->
  Doc
placeAt present written n body continuing = among [] body
  where
    within ctx b d = case d of
      DNest 0 x -> DNest 0 (within ctx b x)
      DNest k x -> DNest k (within ctx body x)
      DAlign x -> DAlign (within ctx body x)
      DGroup l x -> DGroup l (within ctx b x)
      DVariant x y -> DVariant (within ctx b x) (within ctx b y)
      DLocated s x
        | spanStartLine s > n -> among ctx b d
        | DCppChoice{} <- x -> DLocated s (within ctx b x)
        | otherwise -> DLocated s (within ctx (continuing body) x)
      DFence s x -> DFence s (within ctx b x)
      DCppChoice cs bs e
        | present ctx -> d
        | otherwise -> case alternativeAt n cs of
            Everywhere ->
              DCppChoice
                cs
                [(g, among (ctx <> [(cs, k)]) b x) | (k, (g, x)) <- zip [0 ..] bs]
                (among (ctx <> [(cs, length bs)]) b e)
            Only k
              | k < length bs ->
                  DCppChoice
                    cs
                    (replaced k (among (ctx <> [(cs, k)]) b (maybe mempty snd (listToMaybe (drop k bs)))) bs)
                    e
              | otherwise -> DCppChoice cs bs (among (ctx <> [(cs, k)]) b e)
            Nowhere -> d
      _ -> among ctx b d

    among ctx b d
      | present ctx = d
      | otherwise = case break startsAfter (spine d) of
          (before, after)
            | Just (earlier, holder, spacing) <- lastBounded before,
              maybe False (>= n) (endOf holder) ->
                mconcat (earlier <> [within ctx b holder] <> spacing <> after)
            | Just (printed, anchor, spacing) <- lastBounded before,
              Just from <- endOf anchor,
              Just ls <- written ->
                if any (`blankAt` ls) [from .. n - 1] || not (all onlySpacing spacing)
                  then mconcat (before <> [includeWhen (blankAt (n - 1) ls) blankLine, b] <> after)
                  else mconcat (printed <> [anchor, b] <> spacing <> after)
            | otherwise -> mconcat (before <> [b] <> after)

    startsAfter = \case
      DLocated s _ | spanStartColumn s == farRight -> spanStartLine s >= n
      x -> maybe False (> n) (startOf x)

    lastBounded ds = case break (maybe False (const True) . boundsOf) (reverse ds) of
      (spacing, x : earlier) -> Just (reverse earlier, x, reverse spacing)
      _ -> Nothing

    replaced k x bs = [if i == k then (g, x) else (g, y) | (i, (g, y)) <- zip [0 :: Int ..] bs]

    startOf = fmap fst . boundsOf
    endOf = fmap snd . boundsOf

-- | Which alternative of which choice, from the outside in: the
-- configurations a place in a document is printed in.
type Context = [([Conditional], Int)]

-- | Every place a choice printed from a conditional is, as the alternatives
-- leading to it.
realizations :: Conditional -> Doc -> [Context]
realizations c = go []
  where
    go ctx = \case
      DCppChoice cs bs e ->
        [ctx | c `elem` cs]
          <> concat [go (ctx <> [(cs, k)]) d | (k, d) <- zip [0 ..] (fmap snd bs <> [e])]
      d -> foldChildren (go ctx) d

-- | Which alternatives of a choice a line of the input is printed in.
data Alternatives
  = -- | Every one: the line is outside the conditionals.
    Everywhere
  | -- | The one its branch became.
    Only Int
  | -- | None: its branch is never taken.
    Nowhere

-- | Which alternatives of a choice printed from these conditionals a line
-- is printed in.
alternativeAt :: Int -> [Conditional] -> Alternatives
alternativeAt n cs = case branches of
  [] -> Everywhere
  k : ks
    | all (== k) ks -> Only k
    | otherwise -> Nowhere
  where
    branches =
      [ k
      | ls <- fmap conditionalLines cs,
        (k, (from, to)) <- zip [0 ..] (zip ls (drop 1 ls)),
        from < n,
        n < to
      ]

-- | The lines a document was printed from, first and last.
boundsOf :: Doc -> Maybe (Int, Int)
boundsOf = \case
  DLocated s _ -> Just (spanStartLine s, spanEndLine s)
  DFence s _ -> Just (spanStartLine s, spanEndLine s)
  DCppDirective s _ -> Just (spanStartLine s, spanEndLine s)
  DCppChoice cs bs e ->
    hull
      ( mapMaybe conditionalRange cs
          <> [b | Just b <- boundsOf e : fmap (boundsOf . snd) bs]
      )
  DCat a b -> case (boundsOf a, boundsOf b) of
    (Just (from, _), Just (_, to)) -> Just (from, to)
    (found, Nothing) -> found
    (Nothing, found) -> found
  DVariant _ b -> boundsOf b
  d -> boundsOf . snd =<< unwrap d
  where
    hull = \case
      [] -> Nothing
      bs -> Just (minimum (fmap fst bs), maximum (fmap snd bs))
