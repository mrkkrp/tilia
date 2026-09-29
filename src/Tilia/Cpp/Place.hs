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
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Ord (Down (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Comments
  ( Comment (..),
    CommentStyle (..),
    bracketed,
    commentPragma,
    transcendentComment,
  )
import Tilia.Comments.Attach (Margin (..), attachScopedComments)
import Tilia.Cpp.Directives
  ( CppError (..),
    GroupSpec (..),
    Guard (..),
    Opaque (..),
    allGroups,
    gapWritten,
    groupSpec,
    isDirective,
    opSpan,
    opaqueDirectives,
    scanDirectives,
  )
import Tilia.Doc.Combinators (blankLine, hardBreak, includeWhen)
import Tilia.Doc.Internal (Conditional (..), Doc (..))
import Tilia.Source (Lines, Written (..), blankAt, lineAt, linesOf)
import Tilia.Span (Span (..), mkSpan)

-- | What the configurations of a module had besides their code.
data CommentSummary = CommentSummary
  { -- | The comments no syntax tree carries, by where they were written.
    summaryLoose :: Map Span Comment,
    -- | Where every comment was written, whether a tree carries it or not.
    summaryComments :: Set Span,
    -- | Where every Haddock written as @--@ lines was written.
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
      summaryHaddocks =
        Set.fromList
          [ commentSpan c
          | c <- every,
            commentStyle c == DocComment,
            not (bracketed c)
          ]
    }

-- | A conditional of the module as written.
data Group = Group
  { -- | Its directives' lines.
    grConditional :: Conditional,
    -- | Each of its questions, as written after the hash.
    grGuards :: [Guard]
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
  directives <-
    maybe
      (Left UnsplittableConditional)
      Right
      (scanDirectives source)
  let conditionals =
        [ Group
            { grConditional = Conditional (gsOwnLines gs),
              grGuards = gsGuards gs
            }
        | group <- allGroups directives,
          Just gs <- [groupSpec group],
          not (inComment (fst (gsWhole gs)))
        ]
      scope = scopeIn (fmap grConditional conditionals)
      notes =
        [ if any directiveAt [spanStartLine s + 1 .. spanEndLine s - 1]
            then transcendentComment written c
            else c
        | c <- Map.elems (summaryLoose found),
          let s = commentSpan c
        ]
  shelled <- foldM (restoreConditional written) (widened doc) conditionals
  let marked = spacedApart written (realized (widened shelled))
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
  placed <-
    foldM
      (putDirective written)
      noted
      (filter (not . inComment . opLine) (opaqueDirectives source))
  let (under, over) =
        runsInto
          (summaryHaddocks found)
          (Set.fromList [commentSpan c | c <- notes, not (bracketed c)])
          placed
  pure (keptApart under over placed)
  where
    written = linesOf (Written source)
    directiveAt n = maybe False isDirective (lineAt n written)
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
  | -- | A comment written as @--@ lines, and where.
    ANote Span

-- | The comments written as @--@ lines that come out right under a Haddock
-- written the same way, and would be read as more of it; and those that
-- come out right above one, which a comment is kept apart from when the
-- two are written like that.
runsInto ::
  -- | Where the Haddocks written as @--@ lines are.
  Set Span ->
  -- | Where the comments written as @--@ lines are.
  Set Span ->
  Doc ->
  (Set Span, Set Span)
runsInto haddocks notes = snd . go Apart
  where
    go before = \case
      DLocated s x
        | Set.member s notes -> case before of
            AHaddock -> (ANote s, (Set.singleton s, Set.empty))
            _ -> (ANote s, mempty)
        | Set.member s haddocks -> case before of
            ANote n -> (AHaddock, (Set.empty, Set.singleton n) <> snd (go Apart x))
            _ -> (AHaddock, snd (go Apart x))
        | otherwise -> go before x
      DFence _ x -> go before x
      DCat a b ->
        let (between, found) = go before a
            (after, found') = go between b
         in (after, found <> found')
      DNest _ x -> go before x
      DAlign x -> go before x
      DGroup _ x -> go before x
      DVariant _ b -> go before b
      DCppChoice _ bs e -> (Apart, foldMap (snd . go Apart . snd) bs <> snd (go Apart e))
      DEmpty -> (before, mempty)
      DSpace -> (before, mempty)
      DBreak -> (before, mempty)
      DSoftBreak -> (before, mempty)
      DHardBreak -> (before, mempty)
      DCloseLine -> (before, mempty)
      _ -> (Apart, mempty)

-- | Put an empty line above the comments at the first spans, and below
-- those at the second.
keptApart :: Set Span -> Set Span -> Doc -> Doc
keptApart under over = go
  where
    go = \case
      DLocated s x ->
        DLocated s $
          includeWhen (Set.member s under) blankLine
            <> go x
            <> includeWhen (Set.member s over) blankLine
      DFence s x -> DFence s (go x)
      DCat a b -> DCat (go a) (go b)
      DNest n x -> DNest n (go x)
      DAlign x -> DAlign (go x)
      DGroup l x -> DGroup l (go x)
      DVariant a b -> DVariant (go a) (go b)
      DCppChoice cs bs e -> DCppChoice cs [(g, go x) | (g, x) <- bs] (go e)
      d -> d

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
      DNest n x -> let (x', c) = go x in (DNest n x', c)
      DAlign x -> let (x', c) = go x in (DAlign x', c)
      DGroup l x -> let (x', c) = go x in (DGroup l x', c)
      DVariant a b -> let (b', c) = go b in (DVariant (fst (go a)) b', c)
      d -> (d, Nothing)

-- | The lines a conditional was written on, from its @#if@ to its @#endif@.
conditionalSpan :: Conditional -> Maybe Span
conditionalSpan (Conditional ls) = case (ls, unsnoc ls) of
  (from : _, Just (_, to)) -> Just (mkSpan (from, 1) (to, 1))
  _ -> Nothing

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
        | Conditional ls <- conditionals,
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
  DCat a b -> DCat (realized a) (realized b)
  DNest n d -> DNest n (realized d)
  DAlign d -> DAlign (realized d)
  DGroup l d -> DGroup l (realized d)
  DVariant a b -> DVariant (realized a) (realized b)
  DLocated s d -> DLocated s (realized d)
  DFence s d -> DFence s (realized d)
  d -> d

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
      DNest n x -> DNest n (go x)
      DAlign x -> DAlign (go x)
      DGroup l x -> DGroup l (go x)
      DVariant a b -> DVariant (go a) (go b)
      DLocated s x -> DLocated s (go x)
      DFence s x -> DFence s (go x)
      DCppChoice cs bs e -> DCppChoice cs [(g, go x) | (g, x) <- bs] (go e)
      d -> d
    apart = \case
      x : rest
        | Just (_, to) <- ownLines x,
          (_ : _, y : rest') <- span spacing rest,
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
    spacing = \case
      DEmpty -> True
      DSpace -> True
      DBreak -> True
      DSoftBreak -> True
      DHardBreak -> True
      DCloseLine -> True
      _ -> False

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
      DFence s x -> DFence s (rewrite seen x)
      DCat a b -> DCat (rewrite seen a) (rewrite seen b)
      DNest n x -> DNest n (rewrite seen x)
      DAlign x -> DAlign (rewrite seen x)
      DGroup l x -> DGroup l (rewrite seen x)
      DVariant a b -> DVariant (rewrite seen a) (rewrite seen b)
      d -> d
    outermost = go Set.empty
      where
        go seen = \case
          DLocated s x
            | Set.member (startPoint' s) seen -> go seen x
            | otherwise -> s : go (Set.insert (startPoint' s) seen) x
          DFence _ x -> go seen x
          DCat a b -> go seen a <> go seen b
          DNest _ x -> go seen x
          DAlign x -> go seen x
          DGroup _ x -> go seen x
          DVariant _ b -> go seen b
          _ -> []
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
    anchor x (n, s) =
      fromMaybe
        x
        (placeAt (const False) Nothing n (DLocated s mempty) x)
    here n = case alternativeAt n cs of
      Everywhere -> True
      Only i -> i == k
      Nowhere -> False
    anchors (Conditional ls) = case ls of
      opening : _ ->
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
  DCat a b -> spansIn a <> spansIn b
  DNest _ d -> spansIn d
  DAlign d -> spansIn d
  DGroup _ d -> spansIn d
  DVariant _ b -> spansIn b
  DCppChoice _ bs e -> foldMap (spansIn . snd) bs <> spansIn e
  _ -> []

-- | The smallest span covering every region a document records.
regionOf :: Doc -> Maybe Span
regionOf = \case
  DLocated s _ -> Just s
  DFence s _ -> Just s
  DCat a b -> regionOf a <> regionOf b
  DNest _ d -> regionOf d
  DAlign d -> regionOf d
  DGroup _ d -> regionOf d
  DVariant _ b -> regionOf b
  DCppChoice _ bs e -> foldMap (regionOf . snd) bs <> regionOf e
  _ -> Nothing

-- | Make sure a conditional is in the document, putting it where it was
-- written if the merge left it out.
restoreConditional :: Lines -> Doc -> Group -> Either CppError Doc
restoreConditional written doc w =
  maybe (Left UnsplittableConditional) Right $
    placeAt realized' (Just written) opening shell doc
  where
    realized' ctx = any (all (`elem` ctx)) (realizations c doc)
    c@(Conditional ls) = grConditional w
    opening = fromMaybe 0 (listToMaybe ls)
    closing = maybe 0 snd (unsnoc ls)
    guards = fmap guardText (grGuards w)
    shell =
      DCppChoice [c] [(g, mempty) | g <- guards] mempty
        <> includeWhen (blankAt (closing + 1) written) blankLine

-- | Put a directive back where it was written.
putDirective :: Lines -> Doc -> Opaque -> Either CppError Doc
putDirective written doc d =
  maybe (Left (DirectiveUnplaceable [] (T.takeWhile (/= ' ') (opText d)))) Right $
    placeAt (const False) (Just written) (opLine d) body doc
  where
    body =
      DCppDirective (opSpan d) (opText d)
        <> includeWhen (any (`blankAt` written) [opLastLine d, opLastLine d + 1]) blankLine

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
  Doc ->
  Maybe Doc
placeAt present written n body = among []
  where
    within ctx d = case d of
      DNest k x -> DNest k <$> within ctx x
      DAlign x -> DAlign <$> within ctx x
      DGroup l x -> DGroup l <$> within ctx x
      DVariant a b -> DVariant <$> within ctx a <*> within ctx b
      DLocated s x -> DLocated s <$> within ctx x
      DFence s x -> DFence s <$> within ctx x
      DCppChoice cs bs e
        | present ctx -> Just d
        | otherwise -> case alternativeAt n cs of
            Everywhere ->
              DCppChoice cs
                <$> sequenceA [(,) g <$> among (ctx <> [(cs, k)]) x | (k, (g, x)) <- zip [0 ..] bs]
                <*> among (ctx <> [(cs, length bs)]) e
            Only k
              | k < length bs ->
                  (\x -> DCppChoice cs (replaced k x bs) e)
                    <$> among (ctx <> [(cs, k)]) (maybe mempty snd (listToMaybe (drop k bs)))
              | otherwise -> DCppChoice cs bs <$> among (ctx <> [(cs, k)]) e
            Nowhere -> Just d
      _ -> among ctx d

    among ctx d
      | present ctx = Just d
      | otherwise = case break startsAfter (spine d) of
          (before, after)
            | Just (earlier, holder, spacing) <- lastBounded before,
              maybe False (>= n) (endOf holder) ->
                (\x -> mconcat (earlier <> [x] <> spacing <> after)) <$> within ctx holder
            | Just (printed, anchor, spacing) <- lastBounded before,
              Just from <- endOf anchor,
              Just ls <- written ->
                Just $
                  if gapWritten ls (from + 1) (n - 1) || not (all space spacing)
                    then mconcat (before <> [includeWhen (blankAt (n - 1) ls) blankLine, body] <> after)
                    else mconcat (printed <> [anchor, body] <> spacing <> after)
            | otherwise -> Just (mconcat (before <> [body] <> after))

    startsAfter x = maybe False (> n) (startOf x)

    space = \case
      DEmpty -> True
      DSpace -> True
      DBreak -> True
      DSoftBreak -> True
      DHardBreak -> True
      DCloseLine -> True
      _ -> False

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
      DCat a b -> go ctx a <> go ctx b
      DNest _ x -> go ctx x
      DAlign x -> go ctx x
      DGroup _ x -> go ctx x
      DVariant _ b -> go ctx b
      DLocated _ x -> go ctx x
      DFence _ x -> go ctx x
      _ -> []

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
      | Conditional ls <- cs,
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
      ( [ (from, to)
        | Conditional ls <- cs,
          from : _ <- [ls],
          Just (_, to) <- [unsnoc ls]
        ]
          <> [b | Just b <- boundsOf e : fmap (boundsOf . snd) bs]
      )
  DNest _ x -> boundsOf x
  DAlign x -> boundsOf x
  DGroup _ x -> boundsOf x
  DVariant _ b -> boundsOf b
  DCat a b -> case (boundsOf a, boundsOf b) of
    (Just (from, _), Just (_, to)) -> Just (from, to)
    (found, Nothing) -> found
    (Nothing, found) -> found
  _ -> Nothing
  where
    hull = \case
      [] -> Nothing
      bs -> Just (minimum (fmap fst bs), maximum (fmap snd bs))

-- | A document as the sequence of things it concatenates.
spine :: Doc -> [Doc]
spine = \case
  DEmpty -> []
  DCat a b -> spine a <> spine b
  d -> [d]
