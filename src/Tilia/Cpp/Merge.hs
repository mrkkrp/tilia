{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Merging the documents the configurations of a module printed to.
module Tilia.Cpp.Merge
  ( Settled (..),
    merge,
    combine,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (groupBy, maximumBy, sortOn, stripPrefix, transpose, unsnoc)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Cpp.Directives (Guard (..), Varied (..), untouched)
import Tilia.Cpp.Place (regionOf)
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Doc.Combinators qualified as Doc
import Tilia.Doc.Internal
  ( Conditional (..),
    Doc (..),
    Layout (..),
    Wrapper (..),
    conditionalRange,
    foldChildren,
    layoutInside,
    onlyBreaks,
    onlySpacing,
    printedFrom,
    printsNothing,
    spine,
    spineAt,
    unwrap,
    wrap,
  )
import Tilia.Source (Lines, lineAt)
import Tilia.Span
  ( Span,
    covers,
    meets,
    spanStartColumn,
    spanStartLine,
  )

-- | A conditional other than the one asked about that answering a question
-- settles, for some answers at least.
data Settled = Settled
  { -- | The conditional, as written.
    settledConditional :: Conditional,
    -- | The branch each answer settles it on, or 'Nothing' where it is
    -- left open.
    settledBranches :: [Maybe Int]
  }

-- | Merge the documents one conditional's branches printed to.
--
-- A structural walk that keeps what they all agree on and puts a choice
-- where they part.
merge ::
  -- | The module as written.
  Lines ->
  -- | The conditionals asking the question, as written.
  [Conditional] ->
  -- | The question.
  [Guard] ->
  -- | The lines its answer can change.
  Varied ->
  -- | The other conditionals its answers settle.
  [Settled] ->
  -- | One document per answer.
  [Doc] ->
  Doc
merge written conditionals guards varied settledOthers = go Broken
  where
    go _ [] = mempty
    go layout ds@(d : rest)
      | all (agree varied layout d) rest = d
      | Just xs <- traverse only spines = alongside layout xs
      | otherwise = factored layout spines
      where
        spines = fmap (spineAt layout) ds

    alongside _ [] = mempty
    alongside layout xs
      | Just wds <- traverse unwrap xs,
        Just w <- joint wds =
          case (w, go (layoutInside layout w) (fmap snd wds)) of
            (WLocated _, DCppChoice{})
              | spans <- [s | (WLocated s, _) <- wds],
                Just opened <- unwrapping layout spans xs ->
                  opened
            (WGroup l, DCppChoice{})
              | not (all (== l) (layoutsOf wds)),
                d : rest <- fmap snd wds,
                not (all (agree varied l d) rest) ->
                  choice xs
            (WGroup Flat, merged)
              | not (beginsWithChoice merged) -> choice xs
            (_, merged) -> wrap w merged
    alongside layout xs@(x : _) = case x of
      DCppChoice ws bs _
        | Just alternatives <-
            every $ \case
              DCppChoice ws' cs e | ws' == ws, fmap fst cs == fmap fst bs -> Just (fmap snd cs <> [e])
              _ -> Nothing,
          Just (merged, fallback) <- unsnoc (fmap (go layout) (transpose alternatives)) ->
            Doc.cppChoice ws (zip (fmap fst bs) merged) fallback
      _
        | Just spans <- traverse regionOf xs,
          Just opened <- unwrapping layout spans xs ->
            opened
        | otherwise -> choice xs
      where
        every f = traverse f xs

    unwrapping layout spans xs = do
      inside <- sole [i | (i, s) <- zip [0 :: Int ..] spans, all (covers s) spans]
      openedAgainst inside layout =<< listToMaybe (drop inside xs)
      where
        sole [i] = Just i
        sole _ = Nothing

        openedAgainst inside l d = case unwrap d of
          Just (w, x) -> wrap w <$> openedAgainst inside (layoutInside l w) x
          Nothing -> case spineAt l d of
            parts@(_ : _ : _) ->
              Just . factored l $
                [if k == inside then parts else [e] | (k, e) <- zip [0 ..] xs]
            _ -> Nothing

    factored layout ss =
      let same = agree varied layout
          shared = foldl1 (lcs anchoring same) ss
          cut = fmap (segments (anchored same) shared) ss
          stretches = transpose (fmap fst cut)
          anchors = transpose (fmap snd cut)
       in mconcat (woven layout stretches (fmap (go layout) anchors))

    woven layout (s : ss) (c : cs) = foldMap (varying layout) (cutAtConditionals s) : c : woven layout ss cs
    woven layout ss [] = fmap (foldMap (varying layout) . cutAtConditionals) ss
    woven _ [] _ = []

    -- A stretch the configurations disagree over, cut where one conditional
    -- asking the question ends and the next begins, so that each comes out
    -- as a choice of its own rather than one choice printing both. Only
    -- where every element falls inside one of them, in the order they were
    -- written; what lies between them, space aside, is the stretch's own.
    cutAtConditionals ss
      | _ : _ : _ <- ranges,
        Just owners <- traverse (traverse ownerOf) ss,
        present@(first' : _ : _) <- foldr insertOrdered [] [o | Just o <- concat owners],
        let assigned = fmap (settled first') owners,
        all ascending assigned =
          [ [[x | (x, o) <- zip xs os, o == i] | (xs, os) <- zip ss assigned]
          | i <- present
          ]
      | otherwise = [ss]
      where
        ranges =
          mapMaybe
            conditionalRange
            (conditionals <> fmap settledConditional settledOthers)
        ownerOf x = case printedFrom x of
          [] -> Just Nothing
          lines' -> case sortOn fst [r | r@(from, to) <- ranges, all (\(a, b) -> from <= a && b <= to) lines'] of
            outermost : _ -> Just (Just outermost)
            [] -> Nothing
        settled first' os = case [o | Just o <- os] of
          [] -> fmap (const first') os
          o : _ -> drop 1 (scanl (\prev x -> maybe prev id x) o os)
        insertOrdered o os
          | o `elem` os = os
          | otherwise = sortOn fst (o : os)
        ascending os = and (zipWith (<=) os (drop 1 os))

    varying layout ss =
      let (opening, ss1) = sharedStart layout ss
          (ss2, closing) = sharedEnd layout ss1
          (lead, ss3, trail) = hoisted layout ss2
       in mconcat opening
            <> mconcat lead
            <> middle layout ss3
            <> mconcat trail
            <> mconcat closing

    sharedStart layout ss
      | Just (h : hs) <- traverse listToMaybe ss,
        all (agree varied layout h) hs =
          let (c, ss') = sharedStart layout (fmap (drop 1) ss) in (h : c, ss')
      | otherwise = ([], ss)

    sharedEnd layout ss =
      let (c, ss') = sharedStart layout (fmap reverse ss)
          ends = fmap reverse ss'
       in case reverse c of
            x : closing
              | x == Doc.comma,
                all (maybe False (not . onlySpacing . snd) . unsnoc) ends ->
                  (fmap (<> [x]) ends, closing)
            closing -> (ends, closing)

    middle _ [] = mempty
    middle layout ss@(s : rest)
      | all (alike layout s) rest = mconcat s
      | Just xs <- traverse only ss = go layout xs
      | Just (c, tails) <- commaLed layout ss = c <> middle layout tails
      | Just merged <- alongsideHeads layout ss,
        weigh layout merged < weigh layout apart =
          merged
      | otherwise = apart
      where
        apart = choice (fmap (mconcat . commaFirst) ss)

    commaFirst = \case
      x : DBreak : rest | x == Doc.comma -> x : Doc.space : rest
      xs -> xs

    commaLed layout ss = do
      views <- traverse (ledBy layout) ss
      let unmoved = [v | v@(_, _, False) <- views]
      (c, _, _) : _ <- Just (if null unmoved then views else unmoved)
      if length unmoved < length views && all (\(d, _, _) -> agree varied layout c d) views
        then Just (c, [r | (_, r, _) <- views])
        else Nothing

    ledBy layout = \case
      c@DCppChoice{} : r
        | everyAlternative (\xs -> take 1 xs == [Doc.comma]) c -> Just (c, r, False)
      x : DBreak : DCppChoice ws bs e : r
        | x == Doc.comma,
          following@(_ : _) <- dropWhile onlySpacing r,
          everyAlternative (\xs -> fmap snd (unsnoc xs) == Just Doc.comma) (DCppChoice ws bs e) ->
            Just (Doc.cppChoice ws [(g, led b) | (g, b) <- bs] (led e), x : DBreak : following, True)
      _ -> Nothing
      where
        everyAlternative p = \case
          DCppChoice _ bs e -> all (\a -> printsNothing a || p (spineAt layout a)) (e : fmap snd bs)
          _ -> False
        led a
          | printsNothing a = a
          | otherwise = mconcat (Doc.comma : Doc.space : maybe [] fst (unsnoc (spineAt layout a)))

    alongsideHeads layout ss = do
      heads <- traverse listToMaybe ss
      let rest = fmap (drop 1) ss
          (glued, tails) = case traverse (stripPrefix [Doc.comma]) rest of
            Just rest' -> (endingWith Doc.comma, rest')
            Nothing -> (id, rest)
      case heads of
        (h : hs)
          | all (sameKind h) hs,
            all (breaksFirst (lineEnding h hs)) tails ->
              Just
                ( joined
                    (glued (go layout heads))
                    (middle layout tails)
                    (varying layout tails)
                )
        _ -> Nothing
      where
        breaksFirst l t = case dropWhile (quiet l) t of
          [] -> True
          (d : _) -> opensWithBreak layout d
        quiet l d = weigh layout d == 0 && not (opensWithBreak l d)
        lineEnding h hs
          | all ((== printed h) . printed) hs = layout
          | otherwise = Flat
        printed = printDoc defaultRenderOptions

    endingWith t d = fromMaybe (d <> t) (inAlternatives d)
      where
        inAlternatives = \case
          DCppChoice ws bs e
            | not (any printsNothing (e : fmap snd bs)) ->
                Just (DCppChoice ws [(g, endingWith t b) | (g, b) <- bs] (endingWith t e))
          DCat a b
            | printsNothing b -> (<> b) <$> inAlternatives a
            | otherwise -> (a <>) <$> inAlternatives b
          y -> do
            (w, x) <- unwrap y
            wrap w <$> inAlternatives x

    joined before after apart =
      case (choiceAt written Last before, choiceAt written First after) of
        (Just (opening, ws, bs, e, gap), Just (gap', ws', cs, e', closing))
          | fmap fst bs == fmap fst cs,
            ws == ws' || null ws || null ws',
            (g, x) : rest <- bs,
            (_, y) : rest' <- cs,
            (lead, first') <- span breaking (spine (x <> between <> y)) ->
              opening
                <> mconcat lead
                <> Doc.cppChoice
                  (if null ws then ws' else ws)
                  ( (g, mconcat first')
                      : [ (h, z <> between <> z')
                        | ((h, z), (_, z')) <- zip rest rest'
                        ]
                  )
                  (e <> between <> e')
                <> closing
          where
            between = gap <> gap'
        _ -> before <> apart

    sameKind x y = case (unwrap x, unwrap y) of
      (Just (w, t), Just (v, u)) -> kin w v && t /= DEmpty && u /= DEmpty
      _ -> False

    alike layout xs ys =
      length xs == length ys && and (zipWith (agree varied layout) xs ys)

    hoisted layout ss =
      ( widest [l | (l, _, _) <- speaking],
        fmap middleOf peeled,
        widest [r | (_, _, r) <- speaking]
      )
      where
        peeled = fmap peel ss
        speaking = case filter (not . all printsNothing . middleOf) peeled of
          [] -> peeled
          printing -> printing
        middleOf (_, m, _) = m
        widest = \case
          [] -> []
          runs -> maximumBy (comparing (spaceOf layout)) runs

    peel ds =
      let (l, rest) = span breaking ds
          (r, m) = span breaking (reverse rest)
       in (l, reverse m, reverse r)

    breaking = \case
      DDeclarationsStart -> True
      d -> onlyBreaks d

    choice ds
      | (d : _) <- mapMaybe (settledChoice ds) settledOthers = d
    choice ds = case unsnoc ds of
      Just (branches, fallback) ->
        Doc.cppChoice
          (filter (evidenced ds) conditionals)
          (zip (fmap guardText guards) branches)
          fallback
      Nothing -> mempty

    settledChoice ds s = do
      open' <- listToMaybe [d | (d, Nothing) <- zip ds (settledBranches s)]
      (bs, e) <- case filter (not . onlySpacing) (spine open') of
        [DCppChoice cs bs e] | settledConditional s `elem` cs -> Just (bs, e)
        _ -> Nothing
      let printed = maybe open' (\k -> maybe e snd (listToMaybe (drop k bs)))
          printsAlike a b =
            agree varied Broken a b || (printsNothing a && printsNothing b)
      if and (zipWith (printsAlike . printed) (settledBranches s) ds)
        then Just open'
        else Nothing

    evidenced ds c = case conditionalRange c of
      Just (from, to) -> any (\(a, b) -> from < a && b < to) (concatMap printedFrom ds)
      Nothing -> False

    only [d] = Just d
    only _ = Nothing

-- | Would these two documents print the same, laid out like this?
agree :: Varied -> Layout -> Doc -> Doc -> Bool
agree varied layout a b = alike (chunked (spineAt layout a)) (chunked (spineAt layout b))
  where
    alike (Left s : xs) (Left t : ys) = s == t && alike xs ys
    alike (Right x : xs) (Right y : ys) = here x y && alike xs ys
    alike [] [] = True
    alike _ _ = False
    chunked ds =
      let (space, rest) = span onlySpacing ds
       in Left (spaceOf layout space) : case rest of
            [] -> []
            x : more -> Right x : chunked more
    inside x y = agree varied layout x y
    here x y = case (x, y) of
      (DLocated s x', DLocated t y') ->
        s == t
          && ( untouched varied (reach s x') && untouched varied (reach t y')
                 || inside x' y'
             )
      (DCppChoice ws bs x', DCppChoice ws' cs y') ->
        ws == ws'
          && length bs == length cs
          && and [g == h && inside p q | ((g, p), (h, q)) <- zip bs cs]
          && inside x' y'
      _ -> case (unwrap x, unwrap y) of
        (Just (w, x'), Just (v, y')) ->
          w == v && agree varied (layoutInside layout w) x' y'
        _ -> x == y

-- | Does this document, laid out flat, print a choice before anything else?
beginsWithChoice :: Doc -> Bool
beginsWithChoice d = case filter (not . onlySpacing) (spineAt Flat d) of
  DCppChoice{} : _ -> True
  x : _ | Just (_, y) <- unwrap x -> beginsWithChoice y
  _ -> False

-- | A region's span, stretched over the span it prints first, which the
-- Haddock of a constructor, a field or an argument is, written above or
-- below it.
reach :: Span -> Doc -> Span
reach s d = maybe s (<> s) (firstMarked d)
  where
    firstMarked = \case
      DLocated h x -> firstMarked x <|> Just h
      DFence h x -> firstMarked x <|> Just h
      x -> listToMaybe (foldChildren (maybeToList . firstMarked) x)

-- | What a run of space comes to on the page.
data Space = Space !Int !Bool
  deriving (Eq, Ord)

-- | Read a run of space, the way 'Tilia.Doc.Internal.breakLine' does.
spaceOf :: Layout -> [Doc] -> Space
spaceOf layout = go 0 False False
  where
    go ended closed apart = \case
      [] -> Space (min 2 ended) (apart && ended == 0)
      d : ds -> case d of
        DSpace -> go ended closed True ds
        DCloseLine
          | closed -> go ended closed apart ds
          | otherwise -> go (ended + 1) True apart ds
        DCloseLineUnlessAfterOpener _
          | closed -> go ended closed apart ds
          | otherwise -> go (ended + 1) True apart ds
        DHardBreak -> broke ds
        DBreak
          | layout == Broken -> broke ds
          | otherwise -> go ended closed True ds
        DSoftBreak
          | layout == Broken -> broke ds
          | otherwise -> go ended closed apart ds
        _ -> go ended closed apart ds
        where
          broke rest
            | closed = go ended False apart rest
            | otherwise = go (ended + 1) False apart rest

-- | Put several documents' differences from a baseline into one document.
--
-- Each of them is the baseline except inside one conditional's lines, and
-- those lines do not overlap, so their differences can be applied side by
-- side rather than chosen between. Which is what makes varying the
-- conditionals one at a time add up to varying them together, and so what
-- makes the cost linear.
combine :: Layout -> Doc -> [(Varied, Doc)] -> Maybe Doc
combine layout base ds = case filter (\(v, d) -> not (agree v layout base d)) ds of
  [] -> Just base
  [(_, only)] -> Just only
  many -> case (spineAt layout base, [(v, spineAt layout d) | (v, d) <- many]) of
    ([b], ss) | Just xs <- traverse (\(v, s) -> (,) v <$> single s) ss -> descend b xs
    (bs, ss) -> spliced bs ss
  where
    single [d] = Just d
    single _ = Nothing

    descend b xs = do
      own@(_, i) <- unwrap b
      others <- traverse (traverse unwrap) xs
      w <- joint (own : fmap snd others)
      wrap w <$> combine (layoutInside layout w) i (fmap (fmap snd) others)

    spliced bs ss = do
      clustered <-
        traverse
          (cluster bs)
          ( overlapping
              ( sortOn
                  chFrom
                  ( concat
                      [ changesAgainst v (agree v layout) bs s
                      | (v, s) <- ss
                      ]
                  )
              )
          )
      pure (mconcat (applied bs (inWrittenOrder bs clustered)))

    cluster _ [c] = Just c
    cluster bs cs
      | to - from == 1,
        Just xs <- traverse (\c -> (,) (chVaried c) <$> single (chWith c)) cs,
        (b : _) <- drop from bs =
          ( \d ->
              Change
                { chFrom = from,
                  chTo = to,
                  chWith = [d],
                  chVaried = Varied (concatMap (variedLines . chVaried) cs)
                }
          )
            <$> combine layout b xs
      | otherwise = Nothing
      where
        from = minimum (fmap chFrom cs)
        to = maximum (fmap chTo cs)

    applied bs = go 0
      where
        go i [] = drop i bs
        go i (c : cs) =
          take (chFrom c - i) (drop i bs) <> chWith c <> go (chTo c) cs

-- | Put what changes made to one run of space in the order it was written
-- in.
--
-- Two conditionals written one after the other with nothing between them
-- but space both change that space, and where in it each change falls is a
-- matter of how each one's own space lined up with it, not of which came
-- first. Nothing but space moves when the changes trade places.
inWrittenOrder :: [Doc] -> [Change] -> [Change]
inWrittenOrder bs = concatMap reorder . groupBy ((==) `on` gap)
  where
    gap c
      | all onlySpacing (take (chTo c - chFrom c) (drop (chFrom c) bs)) =
          Just (length (filter (not . onlySpacing) (take (chFrom c) bs)))
      | otherwise = Nothing
    reorder cs = case traverse firstLine cs of
      Just ls
        | Just _ <- gap =<< listToMaybe cs,
          ls /= sortOn id ls ->
            zipWith
              (\c w -> c{chWith = chWith w, chVaried = chVaried w})
              cs
              (fmap snd (sortOn fst (zip ls cs)))
      _ -> cs
    firstLine c = case concatMap printedFrom (chWith c) of
      [] -> Nothing
      ls -> Just (minimum (fmap fst ls))

-- | The wrapper standing for those of several documents, if each is of a
-- kind with the first: the smallest span covering theirs for a region, and
-- for a group broken if any that holds something is.
joint :: [(Wrapper, Doc)] -> Maybe Wrapper
joint wds = case fmap fst wds of
  w : ws | all (kin w) ws -> Just $ case w of
    WLocated s -> WLocated (foldr (<>) s [t | WLocated t <- ws])
    WFence s -> WFence (foldr (<>) s [t | WFence t <- ws])
    WGroup _ -> WGroup (if Broken `elem` layoutsOf wds then Broken else Flat)
    _ -> w
  _ -> Nothing

-- | Could what these two wrappers hold be merged under one of them?
kin :: Wrapper -> Wrapper -> Bool
kin a b = case (a, b) of
  (WLocated s, WLocated t) -> meets s t
  (WFence s, WFence t) -> meets s t
  (WGroup _, WGroup _) -> True
  _ -> a == b

-- | The layouts of the groups that hold something.
layoutsOf :: [(Wrapper, Doc)] -> [Layout]
layoutsOf wds = [l | (WGroup l, d) <- wds, not (printsNothing d)]

-- | A stretch of the baseline, and what one document put there instead.
data Change = Change
  { -- | Where the stretch begins, as an index into the baseline.
    chFrom :: !Int,
    -- | Where it ends, one past the last element replaced.
    chTo :: !Int,
    -- | What the document put there instead.
    chWith :: [Doc],
    -- | The lines the conditional this change came from could have reached.
    -- Carried so that a cluster of two of them can be combined without
    -- losing which conditional each half belongs to. See 'Varied'.
    chVaried :: Varied
  }

-- | What one document changed about the baseline, as the stretches it
-- replaced and what it put in each of their places.
changesAgainst ::
  Varied ->
  -- | Whether two elements print the same.
  (Doc -> Doc -> Bool) ->
  [Doc] ->
  [Doc] ->
  [Change]
changesAgainst varied same bs xs = go 0 bs xs (lcs anchoring same bs xs)
  where
    anchor = anchored same

    go i b x [] = between i b x
    go i b x (c : cs) =
      let (b', b'') = break (anchor c) b
          (x', x'') = break (anchor c) x
          j = i + length b'
       in between i b' x'
            <> held j (listToMaybe b'') (listToMaybe x'')
            <> go (j + 1) (drop 1 b'') (drop 1 x'') cs

    held j (Just b') (Just x')
      | not (same b' x') =
          [Change{chFrom = j, chTo = j + 1, chWith = [x'], chVaried = varied}]
    held _ _ _ = []

    between i b x =
      [ Change
          { chFrom = i + shared,
            chTo = i + shared + length b',
            chWith = x',
            chVaried = varied
          }
      | not (null b' && null x')
      ]
      where
        shared = length (takeWhile id (zipWith same b x))
        atEnd = length (takeWhile id (zipWith same (reverse b) (reverse x)))
        kept = min atEnd (min (length b) (length x) - shared)
        b' = take (length b - kept - shared) (drop shared b)
        x' = take (length x - kept - shared) (drop shared x)

-- | Group changes that reach the same stretch of the baseline.
--
-- Ones that merely touch are left apart: a change ending where the next
-- begins has not reached into it.
overlapping :: [Change] -> [[Change]]
overlapping [] = []
overlapping (c : cs) = go [c] (chTo c) cs
  where
    go acc _ [] = [reverse acc]
    go acc end (x : xs)
      | chFrom x < end = go (x : acc) (max end (chTo x)) xs
      | otherwise = reverse acc : go [x] (chTo x) xs

-- | One end of a document.
data Edge = First | Last

-- | The choice a document has at one end, if that is where it has one: what
-- the document prints before the choice, the choice itself, and what the
-- document prints after it.
choiceAt ::
  -- | The module as written.
  Lines ->
  -- | The end to look at.
  Edge ->
  -- | The document.
  Doc ->
  Maybe (Doc, [Conditional], [(Text, Doc)], Doc, Doc)
choiceAt written edge = go Flat False
  where
    go layout fresh d = case span onlySpacing (inward (spine d)) of
      (outer, x : inner) ->
        let (before, after) = case edge of
              First -> (mconcat outer, mconcat inner)
              Last -> (mconcat (reverse inner), mconcat (reverse outer))
            around w v (b, ws, bs, e, a) =
              ( before <> printed w b,
                ws,
                fmap (fmap (printed v)) bs,
                printed v e,
                printed w a <> after
              )
            printed w y = if printsNothing y then y else w y
            begins = case (edge, inner) of
              (First, _) -> False
              (Last, []) -> fresh
              (Last, y : _) | Space ended _ <- spaceOf layout [y] -> ended > 0
         in case x of
              DCppChoice ws bs e -> Just (before, ws, bs, e, after)
              _ -> do
                (w, y) <- unwrap x
                guard (w /= WAlign || begins && firstOnItsLine y)
                around (wrap w) (alternatives w)
                  <$> go (layoutInside layout w) begins y
      _ -> Nothing
    inward = case edge of
      First -> id
      Last -> reverse
    alternatives = \case
      WLocated _ -> id
      WFence _ -> id
      w -> wrap w
    firstOnItsLine y = case regionOf y of
      Just s ->
        maybe
          False
          (T.all isSpace . T.take (spanStartColumn s - 1))
          (lineAt (spanStartLine s) written)
      Nothing -> False

-- | Does the first thing this document puts on the page end a line?
opensWithBreak :: Layout -> Doc -> Bool
opensWithBreak layout d = case dropWhile (== DSpace) (spineAt layout d) of
  x : _
    | Just (w, y) <- unwrap x -> opensWithBreak (layoutInside layout w) y
    | otherwise -> case x of
        DHardBreak -> True
        DCloseLine -> True
        DCloseLineUnlessAfterOpener _ -> True
        DBreak -> layout == Broken
        DSoftBreak -> layout == Broken
        DCppDirective _ _ -> True
        DCppChoice{} -> True
        _ -> False
  [] -> False

-- | How much text this document holds, counting what a choice repeats once
-- for each alternative that repeats it.
weigh :: Layout -> Doc -> Int
weigh layout = go
  where
    go = \case
      DCat a b -> go a + go b
      DVariant flatD brokenD ->
        go (case layout of Flat -> flatD; Broken -> brokenD)
      DCppChoice _ bs e -> sum (fmap (go . snd) bs) + go e
      DText t -> T.length t
      DCppDirective _ t -> T.length t
      DHoldBack _ t -> T.length t
      d
        | Just (w, x) <- unwrap d -> weigh (layoutInside layout w) x
        | otherwise -> 0

-- | The longest run of elements two spines have in common, in order,
-- allowing for anything either of them has that the other does not, of
-- those that could hold them together.
lcs :: (a -> Bool) -> (a -> a -> Bool) -> [a] -> [a] -> [a]
lcs holds same xs ys =
  filter holds opening <> table middleX middleY <> filter holds closing
  where
    agreeing as bs = length (takeWhile id (zipWith same as bs))

    ahead = agreeing xs ys
    (opening, xs1) = splitAt ahead xs
    ys1 = drop ahead ys
    behind = agreeing (reverse xs1) (reverse ys1)
    (middleX, closing) = splitAt (length xs1 - behind) xs1
    middleY = take (length ys1 - behind) ys1

    table [] _ = []
    table _ [] = []
    table as bs = reverse (snd (last (foldl' (row bs) (start bs) as)))
      where
        start cs = replicate (length cs + 1) (0 :: Int, [])
        row cs previous x = cells 0 [] (zip3 cs previous (drop 1 previous))
          where
            cells !n acc rest =
              (n, acc) : case rest of
                [] -> []
                ((y, (dn, ds), (an, as')) : more)
                  | holds x, same x y -> cells (dn + 1) (x : ds) more
                  | n >= an -> cells n acc more
                  | otherwise -> cells an as' more

-- | Only let something that was printed line two spines up.
anchored :: (Doc -> Doc -> Bool) -> Doc -> Doc -> Bool
anchored same a b = anchoring a && same a b

-- | Could this element hold two spines together, if it turned up in both?
anchoring :: Doc -> Bool
anchoring = \case
  DVerbatimBreak _ _ -> False
  DText "," -> False
  DDeclarationsStart -> False
  DNest _ d -> located d || not (printsNothing d)
  DAlign d -> located d || not (printsNothing d)
  DGroup _ d -> located d || not (printsNothing d)
  d -> not (onlySpacing d)
  where
    located = \case
      DLocated{} -> True
      DFence{} -> True
      DCat a b -> located a || located b
      DNest _ d -> located d
      DAlign d -> located d
      DGroup _ d -> located d
      _ -> False

-- | A spine cut at the elements it shares with the others: one stretch
-- before each of them, and one after the last, and the elements themselves.
--
-- The matched elements come back rather than being dropped because the
-- caller cannot assume they are interchangeable: 'agree' does not look
-- inside a region the conditional leaves alone.
segments ::
  -- | Whether an element of the spine is the shared one being looked for.
  (a -> a -> Bool) ->
  -- | The shared elements, in order, to cut at.
  [a] ->
  -- | The spine to cut.
  [a] ->
  -- | The stretches between the cuts, and the elements cut at.
  ([[a]], [a])
segments same = go
  where
    go [] s = ([s], [])
    go (c : cs) s = case break (same c) s of
      (before', matched : rest) -> keeping before' matched (go cs rest)
      (before', []) -> keeping before' c (go cs [])
      where
        keeping before' matched (stretches, anchors) =
          (before' : stretches, matched : anchors)
