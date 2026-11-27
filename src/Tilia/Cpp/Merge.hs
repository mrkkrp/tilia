{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
import Data.List
  ( find,
    groupBy,
    isPrefixOf,
    maximumBy,
    minimumBy,
    nub,
    sortOn,
    transpose,
    uncons,
    unsnoc,
  )
import Data.Maybe
  ( catMaybes,
    fromMaybe,
    isJust,
    listToMaybe,
    mapMaybe,
    maybeToList,
  )
import Data.Monoid (All (..), Any (..))
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Tilia.Cpp.Directives (Guard, Varied (..), guardText, untouched)
import Tilia.Doc
  ( Conditional (..),
    Doc (..),
    Layout (..),
    SeparatorSide (..),
    Wrapper (..),
    choosing,
    conditionalRange,
    foldChildren,
    foldRegions,
    layoutInside,
    mapChildren,
    namesOnly,
    onlyBreaks,
    onlySpacing,
    printedFrom,
    printsNothing,
    printsOnOneLine,
    referentIn,
    regionOf,
    separatorSide,
    spine,
    spineAt,
    unwrap,
    wrap,
    pattern Cat,
  )
import Tilia.Doc.Combinators qualified as Doc
import Tilia.Source (Lines, lineAt, offsetOf)
import Tilia.Span (Span (..), covers, endOf, meets, startOf)

-- | A conditional other than the one asked about that answering a question
-- settles, for some answers at least.
data Settled = Settled
  { -- | The conditional, as written.
    settledConditional :: Conditional,
    -- | The branch each answer settles it on, though it may print the
    -- conditional whole, or 'Nothing' where it leaves it open.
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
    go layout ds@(d : _)
      | s : others <- spines,
        all (agreeAll varied layout s) others =
          foldl aliased d (drop 1 ds)
      | Just xs <- traverse sole spines = alongside layout xs
      | otherwise = factored middle layout spines
      where
        spines = fmap (spineAt layout) ds

    alongside _ [] = mempty
    alongside layout xs
      | Just wds <- traverse unwrap xs,
        Just w <- joint (fmap fst wds) =
          case (w, go (layoutInside layout w) (fmap snd wds)) of
            (WLocated _, _)
              | spans <- [s | (WLocated s, _) <- wds],
                Just opened <- unwrapping layout spans xs (coveredAmong spans) ->
                  opened
            (WGroup l, merged)
              | l /= Broken,
                not (beginsWithChoice merged),
                not (any printsLeftAlone xs) ->
                  choice layout $ case fmap regionOf xs of
                    r : rs | all (== r) rs -> xs
                    _ -> fmap Doc.infrangible xs
            (WGroup _, merged)
              | ls@(l : more) <- [l | (WGroup l, _) <- wds],
                any (/= l) more ->
                  wrap w (laidOutAsTheirs ls merged)
            (_, merged) -> wrap w merged
    alongside layout xs@(x : _) = case x of
      DCppChoice ws bs _
        | Just alternatives <-
            every $ \case
              DCppChoice ws' cs e
                | ws' == ws,
                  fmap fst cs == fmap fst bs ->
                    Just (fmap snd cs <> [e])
              _ -> Nothing,
          Just (merged, fallback) <-
            unsnoc . fmap (go layout) $
              zipWith (asTaken ws) [0 ..] (transpose alternatives) ->
            Doc.cppChoice ws (zip (fmap fst bs) merged) fallback
      _
        | any (isJust . ungrouped) xs ->
            go layout [fromMaybe d (ungrouped d) | d <- xs]
        | Just spans <- traverse regionOf xs,
          Just opened <- unwrapping layout spans xs (const True) ->
            opened
        | otherwise -> choice layout xs
      where
        every f = traverse f xs
        ungrouped = \case
          DGroup Broken d | layout == Broken -> Just d
          _ -> Nothing

    -- Does this document print anything from lines the conditional left
    -- alone?
    printsLeftAlone =
      getAny
        . foldRegions
          ( \s d inside ->
              inside
                <> Any
                  ( not (printsNothing d)
                      && untouched varied (spanWithHaddock s d)
                  )
          )

    -- A choice among the configurations whose groups these layouts are,
    -- each alternative laid out as its configuration lays the group out.
    laidOutAsTheirs ls = \case
      DCppChoice cs bs e
        | fmap fst bs `isPrefixOf` fmap guardText guards,
          Just (front, l) <- unsnoc ls,
          length front == length bs ->
            DCppChoice cs (zipWith (fmap . DGroup) front bs) (DGroup l e)
      d@DCppChoice{} -> d
      d@DGroup{} -> d
      d -> mapChildren (laidOutAsTheirs ls) d

    unwrapping layout spans xs accepted = do
      inside <-
        sole [i | (i, s) <- zip [0 :: Int ..] spans, all (covers s) spans]
      openedAgainst inside layout =<< listToMaybe (drop inside xs)
      where
        openedAgainst inside l d = case unwrap d of
          Just (w, x)
            | indenting w -> Nothing
            | otherwise -> wrap w <$> openedAgainst inside (layoutInside l w) x
          Nothing -> case spineAt l d of
            parts@(_ : _ : _)
              | let opened = tucked inside l parts,
                accepted opened ->
                  Just . factored middle l $
                    [ if k == inside then opened else [e]
                    | (k, e) <- zip [0 ..] xs
                    ]
            _ -> Nothing

        tucked inside l parts = case unsnoc parts of
          Just (front, x) | Just tucked' <- under l x -> front <> tucked'
          _ -> parts
          where
            column = spanStartColumn <$> listToMaybe (drop inside spans)
            under l' x = do
              (w, y) <- unwrap x
              guard (indenting w)
              let l'' = layoutInside l' w
                  (spacing, code) = span onlySpacing (spineAt l'' y)
              case [ e
                   | e <- xs,
                     regionOf e == regionOf y,
                     fmap spanStartColumn (regionOf e) == column,
                     untouchedRegion varied e
                   ] of
                e : _ -> Just (spacing <> [e])
                [] -> do
                  (front, z) <- unsnoc code
                  (wrap w (mconcat (spacing <> front)) :) <$> under l'' z

    factored within layout ss =
      let same (x, _) (y, _) = agree varied layout x y
          common as bs =
            [ (x, is <> js)
            | ((x, is), (_, js)) <- lcs (anchoring . fst) same as bs
            ]
          shared = foldl1 common [zip s (pure <$> [0 :: Int ..]) | s <- ss]
          cut =
            [cutAt [is !! k | (_, is) <- shared] s | (k, s) <- zip [0 ..] ss]
          stretches = transpose (fmap fst cut)
          anchors = transpose (fmap snd cut)
       in mconcat (woven within layout stretches (fmap (go layout) anchors))

    -- The stretches between the parts every spine shares, merged, each
    -- taking in the part after it where it ends with differing separators
    -- leading that, and the trailing separator the part begins with.
    woven within layout (s : ss) (c : cs)
      | Just leads <- traverse ledInto s,
        (l : ls) <- leads,
        any (/= l) ls,
        s' : ss' <- ss =
          woven within layout (zipWith (<>) (fmap (<> [c]) s) s' : ss') cs
      | any (not . null) s,
        Just (t, c') <- separatorFirst c =
          woven within layout (fmap (<> [t]) s : ss) (c' : cs)
      | otherwise =
          foldMap (varying within layout) (cutAtConditionals s)
            : c
            : woven within layout ss cs
    woven within layout ss [] =
      fmap (foldMap (varying within layout) . cutAtConditionals) ss
    woven _ _ [] _ = []

    -- The trailing separator a part begins with, in the indentation it was
    -- written in or as the referent the part is indented from, and the rest.
    separatorFirst = \case
      DIndented n a d
        | separatorSide a == Just TrailingSeparator ->
            Just (a, DIndented n (Doc.ptr a) d)
        | t : rest <- spine d,
          separatorSide t == Just TrailingSeparator ->
            Just (DIndented n a t, DIndented n a (mconcat rest))
      _ -> Nothing

    -- The leading separator a stretch ends with, spacing aside.
    ledInto s = case dropWhile onlySpacing (reverse s) of
      x : _ | separatorSide x == Just LeadingSeparator -> Just x
      _ -> Nothing

    -- A stretch the configurations disagree over, cut where one conditional
    -- asking the question ends and the next begins, so that each comes out
    -- as a choice of its own rather than one choice printing both. Only
    -- where every element falls inside one of them, in the order they were
    -- written; what lies between them, space aside, is the stretch's own.
    cutAtConditionals ss
      | _ : _ : _ <- ranges,
        Just owners <- traverse (traverse ownerOf) ss,
        present@(first' : _ : _) <-
          foldr insertOrdered [] [o | Just o <- concat owners],
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
          lines' -> case sortOn
            fst
            [ r
            | r@(from, to) <- ranges,
              all (\(a, b) -> from <= a && b <= to) lines'
            ] of
            outermost : _ -> Just (Just outermost)
            [] -> Nothing
        settled first' os = case [o | Just o <- os] of
          [] -> fmap (const first') os
          o : _ -> drop 1 (scanl (\prev x -> maybe prev id x) o os)
        insertOrdered o os
          | o `elem` os = os
          | otherwise = sortOn fst (o : os)
        ascending os = and (zipWith (<=) os (drop 1 os))

    varying within layout ss =
      let (opening, ss1) = sharedStart layout ss
          (ss2, closing) = sharedEnd layout ss1
       in fromMaybe
            (mconcat opening <> disagreement ss2)
            (separatorAhead opening ss2)
            <> mconcat closing
      where
        disagreement ss2 =
          let (lead, ss3, trail) = hoisted layout ss2
           in mconcat lead
                <> within layout ss3
                <> mconcat trail

        -- The separator every configuration leads what they disagree over
        -- with, after the break they share, on its other side at the end of
        -- the line before them where one of them has nothing there.
        separatorAhead opening ss2 = do
          heads <- traverse ledBySeparator ss2
          (a, _) : others <- Just heads
          guard (all ((== a) . fst) others)
          (front, b) <- unsnoc opening
          guard (onlyBreaks b)
          let d = disagreement (fmap snd heads)
          guard (maybe False (any printsNothing) (leadingAlternatives d))
          Just (mconcat front <> otherSide a <> b <> d)

        ledBySeparator s = case dropWhile emptyRegion s of
          a : more | separatorSide a == Just LeadingSeparator -> Just (a, more)
          _ -> Nothing

    sharedStart = sharedAlong LeadingSeparator

    -- What every spine begins with, a separator only together with the
    -- part it goes with where that lies ahead, a part never apart from the
    -- differing separators it goes with, and seen past a choice where only
    -- that makes them agree.
    sharedAlong ahead layout ss
      | Just (h : hs) <- traverse listToMaybe ss,
        all (agree varied layout h) hs,
        not ledApart,
        (c, ss') <- sharedAlong ahead layout (fmap (drop 1) ss),
        separatorSide h /= Just ahead || not (null c) =
          (h : c, ss')
      | moved <- fmap (separatorPast ahead layout) ss,
        any isJust moved,
        let ss' = zipWith fromMaybe ss moved,
        Just (h : hs) <- traverse listToMaybe ss',
        all (agree varied layout h) hs =
          sharedAlong ahead layout ss'
      | otherwise = ([], ss)
      where
        ledApart =
          case traverse (listToMaybe . dropWhile onlySpacing . drop 1) ss of
            Just (x : xs) ->
              all ((== Just behind) . separatorSide) (x : xs) && any (/= x) xs
            _ -> False
        behind = case ahead of
          LeadingSeparator -> TrailingSeparator
          TrailingSeparator -> LeadingSeparator

    -- A spine seen with the separator by the choice it begins with, before
    -- it or at the far end of every alternative, past the choice instead.
    separatorPast ahead layout = \case
      s : rest
        | isJust (separatorSide s),
          (spacing, c : rest') <- span onlySpacing rest,
          Just (_, c') <- through (Just s) layout c ->
            Just (c' : s : spacing <> dropWhile onlySpacing rest')
      c : rest
        | Just (t, c') <- through Nothing layout c -> Just (c' : t : rest)
      _ -> Nothing
      where
        through near l = \case
          DCppChoice ws bs e -> do
            (t, e') <- past near l e
            bs' <- traverse (traverse (past near l)) bs
            first' : more <- Just (catMaybes (t : fmap (fst . snd) bs'))
            guard (all (same first') more)
            Just (first', Doc.cppChoice ws (fmap (fmap snd) bs') e')
          d -> do
            (w, x) <- unwrap d
            fmap (wrap w) <$> through near (layoutInside l w) x
        past near l a
          | printsNothing a = (Nothing, a) <$ near
          | otherwise = do
              (t, inner) <- farEnd (spineAt l a)
              guard (isJust (separatorSide t) && all (same t) near)
              Just
                ( Just t,
                  mconcat (nearEnd (onSide ahead <$> maybeToList near) inner)
                )
        farEnd xs = case ahead of
          LeadingSeparator -> (\(front, t) -> (t, front)) <$> unsnoc xs
          TrailingSeparator -> uncons xs
        nearEnd n inner = case ahead of
          LeadingSeparator -> n <> inner
          TrailingSeparator -> inner <> n
        same x y = onSide LeadingSeparator x == onSide LeadingSeparator y

    sharedEnd layout ss =
      let (c, ss') = sharedAlong TrailingSeparator layout (fmap reverse ss)
       in (fmap reverse ss', reverse c)

    middle _ [] = mempty
    middle layout ss
      | Just xs <- traverse sole ss = go layout xs
      | Just (c, rests) <- separated LeadingSeparator ss =
          onEdge First c (middle layout rests)
      | Just (c, rests) <- separated TrailingSeparator ss =
          onEdge Last c (middle layout rests)
      | otherwise =
          minimumBy
            (comparing (weigh layout))
            (apart : maybeToList (alongsideHeads layout ss) <> byLines)
      where
        apart = choice layout (fmap mconcat ss)
        byLines
          | not (any continuing ss),
            not (null (foldr1 Set.intersection (foldMap writtenAt <$> ss))),
            (ls, cuts) <- unzip (fmap (printedLines layout) ss),
            or cuts =
              [factored whole layout ls]
          | otherwise = []
        continuing = \case
          x : xs
            | printsNothing x -> continuing xs
            | separatorSide x == Just TrailingSeparator -> False
            | Just (_, y) <- unwrap x -> continuing (spine y <> xs)
            | otherwise -> x == DSpace
          [] -> False

    -- The separator every stretch begins or ends with, on the side it is
    -- written on, and what each holds besides.
    separated side ss = do
      views@((c, _) : more) <- traverse edgeOf ss
      guard (all ((== c) . fst) more)
      let rests = fmap snd views
      guard (not (any (all onlySpacing) rests))
      Just (c, rests)
      where
        edgeOf s = case side of
          LeadingSeparator
            | c : r <- s,
              separatorSide c == Just LeadingSeparator ->
                Just (c, r)
          TrailingSeparator
            | Just (r, c) <- unsnoc s,
              separatorSide c == Just TrailingSeparator ->
                Just (c, r)
          _ -> Nothing

    whole _ [] = mempty
    whole layout ss@(s : rest)
      | all (alike layout s) rest = mconcat s
      | Just splits <- traverse firstPart ss,
        not (any (\(_, x, _) -> printsOnOneLine layout x) splits),
        let inside =
              go layout [mconcat (spacing <> [x]) | (spacing, x, _) <- splits]
                <> whole layout [more | (_, _, more) <- splits],
        weigh layout inside < weigh layout apart =
          inside
      | otherwise = apart
      where
        apart = choice layout (fmap mconcat ss)

    firstPart parts = case break (not . blank) parts of
      (spacing, x : more) -> Just (spacing, x, more)
      _ -> Nothing

    -- An alternative with the trailing separator it begins with, inside
    -- whatever wraps its start, written leading the line it ends, or with
    -- the leading separator it ends with put at the end of the line before.
    separatorGlued xs = fromMaybe (gluedEnd xs) (gluedStart xs)

    gluedStart = \case
      x : DBreak : rest
        | separatorSide x == Just TrailingSeparator ->
            Just (otherSide x : rest)
      x : DIndented n a d : rest
        | separatorSide x == Just TrailingSeparator,
          DBreak : more <- spine d ->
            Just (otherSide x : DIndented n a (mconcat more) : rest)
      x : rest
        | onlySpacing x -> (x :) <$> gluedStart rest
        | separatorSide x /= Just TrailingSeparator,
          Just (w, y) <- unwrap x ->
            (: rest) . wrap w . mconcat <$> gluedStart (spine y)
      _ -> Nothing

    gluedEnd xs = case reverse xs of
      x : rest
        | separatorSide x == Just LeadingSeparator,
          (_ : _, rest') <- span blank rest ->
            reverse (gluedTo [otherSide x] rest')
      _ -> xs

    gluedTo glue = \case
      d : DHardBreak : rest -> d : DHardBreak : glue <> rest
      rest -> glue <> rest

    alongsideHeads layout ss = do
      heads <- traverse listToMaybe ss
      let tails = fmap (drop 1) ss
      case heads of
        (h : hs)
          | all (sameKind h) hs,
            all (breaksFirst lineEnding) tails ->
              Just
                ( joined
                    merged
                    (middle layout tails)
                    (varying middle layout tails)
                )
          where
            merged = go layout heads
            lineEnding = if choosing merged then Flat else layout
        _ -> Nothing
      where
        breaksFirst l t = case dropWhile (quiet l) t of
          [] -> True
          (d : _) -> opensWithBreak layout d
        quiet l d = weigh layout d == 0 && not (opensWithBreak l d)

    onEdge edge t d = fromMaybe (beside d) (inAlternatives d)
      where
        beside = case edge of
          First -> (t <>)
          Last -> (<> t)
        inAlternatives = \case
          DCppChoice ws bs e
            | not (any printsNothing (e : fmap snd bs)) ->
                Just
                  ( DCppChoice
                      ws
                      [(g, onEdge edge t b) | (g, b) <- bs]
                      (onEdge edge t e)
                  )
          Cat a b -> case edge of
            First
              | printsNothing a -> (a <>) <$> inAlternatives b
              | otherwise -> (<> b) <$> inAlternatives a
            Last
              | printsNothing b -> (<> b) <$> inAlternatives a
              | otherwise -> (a <>) <$> inAlternatives b
          y -> do
            (w, x) <- unwrap y
            guard (keepsColumn w x)
            wrap w <$> inAlternatives x
        keepsColumn w x = case edge of
          First -> not (aligning w) || onlyChoice x
          Last -> True

    joined before after apart =
      case (choiceAt written Last before, choiceAt written First after) of
        (Just (opening, ws, bs, e, gap), Just (gap', ws', cs, e', closing))
          | fmap fst bs == fmap fst cs,
            ws == ws',
            (g, x) : rest <- bs,
            (_, y) : rest' <- cs,
            (lead, first') <- span breaking (spine (x <> between <> y)) ->
              opening
                <> mconcat lead
                <> Doc.cppChoice
                  ws
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

    sameKind x y = case (unwrap (unreferred x), unwrap (unreferred y)) of
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
          runs -> maximumBy (comparing width) runs
        width r = (spaceOf layout r, DDeclarationsStart `elem` r)

    peel ds =
      let (l, rest) = span breaking ds
          (r, m) = span breaking (reverse rest)
       in (l, reverse m, reverse r)

    breaking = \case
      DDeclarationsStart -> True
      Cat a b -> breaking a && breaking b
      DGroup _ d -> not (printsNothing d) && breaking d
      d
        | Just (w, x) <- unwrap d,
          indenting w ->
            not (printsNothing x) && breaking x
        | otherwise -> onlyBreaks d

    choice layout ds
      | (d : _) <- mapMaybe (settledChoice layout ds) settledOthers = d
    choice _ ds
      | all blank ds = fromMaybe mempty (find (not . printsNothing) ds)
    choice _ ds = case unsnoc
      (fmap (mconcat . separatorGlued . spine) ds) of
      Just (branches, fallback) ->
        Doc.cppChoice
          (filter (evidenced ds) conditionals)
          (zip (fmap guardText guards) branches)
          fallback
      Nothing -> mempty

    settledChoice layout ds s =
      listToMaybe
        [ open'
        | open' <- ds,
          holds open',
          and (zipWith (fits open') (settledBranches s) ds)
        ]
      where
        fits open' k d = alike layout (inLines open') (inLines d)
          where
            inLines =
              fst . printedLines layout . spineAt layout . maybe id taking k
        holds = \case
          DCppChoice cs _ _ | settled cs -> True
          d -> getAny (foldChildren (Any . holds) d)
        taking k = \case
          DCppChoice cs bs e
            | settled cs -> maybe e snd (listToMaybe (drop k bs))
          d -> mapChildren (taking k) d
        settled = elem (settledConditional s)

    asTaken ws k alternatives = fromMaybe alternatives $ do
      s <- find ((`elem` ws) . settledConditional) settledOthers
      let taking = fmap (all (== k)) (settledBranches s)
      taken <- listToMaybe [a | (a, True) <- zip alternatives taking]
      Just [if t then a else taken | (a, t) <- zip alternatives taking]

    evidenced ds c = case conditionalRange c of
      Just (from, to) ->
        any (\(a, b) -> from < a && b < to) (concatMap printedFrom ds)
      Nothing -> False

-- | The one element of a list that has one.
sole :: [a] -> Maybe a
sole = \case
  [x] -> Just x
  _ -> Nothing

-- | Would these two documents print the same, laid out like this?
agree :: Varied -> Layout -> Doc -> Doc -> Bool
agree varied layout a0 b0
  | lone a, lone b = agreeOn varied layout a b
  | otherwise = agreeAll varied layout (spineAt layout a) (spineAt layout b)
  where
    a = unreferred a0
    b = unreferred b0
    lone = \case
      Cat _ _ -> False
      DVariant{} -> False
      d -> not (onlySpacing d)

-- | Would these two spines print the same, laid out like this?
agreeAll :: Varied -> Layout -> [Doc] -> [Doc] -> Bool
agreeAll varied layout xs ys =
  spaceOf layout space == spaceOf layout space' && case (rest, rest') of
    ([], []) -> True
    (x : more, y : more') ->
      agreeOn varied layout x y && agreeAll varied layout more more'
    _ -> False
  where
    (space, rest) = span onlySpacing xs
    (space', rest') = span onlySpacing ys

-- | Would these two elements of spines print the same, laid out like this?
agreeOn ::
  -- | The lines the answer can change.
  Varied ->
  -- | The layout of the group they are printed in.
  Layout ->
  -- | The first.
  Doc ->
  -- | The second.
  Doc ->
  Bool
agreeOn varied layout x0 y0 = case (x, y) of
  (DLocated s x', DLocated t y') ->
    s == t
      && ( untouchedRegion varied x
             && untouchedRegion varied y
             || inside x' y'
         )
  (DCppChoice ws bs x', DCppChoice ws' cs y') ->
    ws == ws'
      && length bs == length cs
      && and [g == h && inside p q | ((g, p), (h, q)) <- zip bs cs]
      && inside x' y'
  (DFence s x', DFence t y') -> meets s t && inside x' y'
  _ -> case (unwrap x, unwrap y) of
    (Just (w, x'), Just (v, y')) ->
      samePlacing w v && agree varied (layoutInside layout w) x' y'
    _ -> x == y
  where
    inside = agree varied layout
    x = unreferred x0
    y = unreferred y0

-- | A part two configurations agree on, as the first prints it, also
-- named as the second names it and each relation around it also naming
-- the referents the second's names.
aliased ::
  -- | The first.
  Doc ->
  -- | The second.
  Doc ->
  Doc
aliased x y = case (unwrap x, unwrap y) of
  (Just (w@WIndented{}, x'), Just (v, y'))
    | Just j <- joint [w, v] -> wrap j (aliased x' y')
  (_, Just (WReferent k, y'))
    | referentIn x /= Just k -> DReferent k (aliased x y')
  _ -> x

-- | A document less the marks that make it a referent, which print it as
-- it is.
unreferred ::
  -- | The document.
  Doc ->
  Doc
unreferred = \case
  DReferent _ d -> unreferred d
  d -> d

-- | Is this a region printed from lines the conditional left alone?
untouchedRegion ::
  -- | The lines the answer can change.
  Varied ->
  -- | The document.
  Doc ->
  Bool
untouchedRegion varied = \case
  DLocated s x -> untouched varied (spanWithHaddock s x)
  _ -> False

-- | Is this document one choice, in nothing but wrappers?
onlyChoice :: Doc -> Bool
onlyChoice = \case
  DCppChoice{} -> True
  d -> maybe False (onlyChoice . snd) (unwrap d)

-- | Is one of these regions, but the one covering them all, among these
-- parts?
coveredAmong ::
  -- | The regions.
  [Span] ->
  -- | The parts.
  [Doc] ->
  Bool
coveredAmong spans = any (maybe False covered . regionOf)
  where
    covered s = s `elem` spans && not (all (covers s) spans)

-- | Does this document, laid out flat, print a choice before anything else?
beginsWithChoice :: Doc -> Bool
beginsWithChoice = isJust . leadingAlternatives

-- | The alternatives of the choice this document, laid out flat, prints
-- before anything else.
leadingAlternatives :: Doc -> Maybe [Doc]
leadingAlternatives d = case filter (not . onlySpacing) (spineAt Flat d) of
  DCppChoice _ bs e : _ -> Just (e : fmap snd bs)
  x : _ | Just (_, y) <- unwrap x -> leadingAlternatives y
  _ -> Nothing

-- | A region's span, stretched over the span it prints first, which the
-- Haddock of a constructor, a field or an argument is, written above or
-- below it.
spanWithHaddock :: Span -> Doc -> Span
spanWithHaddock s d = maybe s (<> s) (firstMarked d)
  where
    firstMarked = \case
      DLocated h x -> firstMarked x <|> Just h
      DFence h x -> firstMarked x <|> Just h
      x -> listToMaybe (foldChildren (maybeToList . firstMarked) x)

-- | What a run of space comes to on the page.
data Space = Space !Int !Bool
  deriving (Eq, Ord)

-- | Read a run of space, in the wrappers it is printed in, the way
-- 'Tilia.Doc.Internal.breakLine' does.
spaceOf ::
  -- | The layout of the group it is printed in.
  Layout ->
  -- | The run.
  [Doc] ->
  Space
spaceOf layout = go 0 False False
  where
    go ended closed apart = \case
      [] -> Space (min 2 ended) (apart && ended == 0)
      d : ds -> case d of
        DSpace -> go ended closed True ds
        DCloseLine
          | closed -> go ended closed apart ds
          | otherwise -> go (ended + 1) True apart ds
        DHardBreak -> broke ds
        DBreak
          | layout == Broken -> broke ds
          | otherwise -> go ended closed True ds
        DSoftBreak
          | layout == Broken -> broke ds
          | otherwise -> go ended closed apart ds
        _
          | Just wrapped <- unwrap d ->
              go ended closed apart (resolved layout wrapped <> ds)
          | otherwise -> go ended closed apart ds
        where
          broke rest
            | closed = go ended False apart rest
            | otherwise = go (ended + 1) False apart rest
    resolved l (w, x) = concatMap printed (spineAt inside x)
      where
        inside = layoutInside l w
        printed = \case
          DBreak -> [if inside == Broken then DHardBreak else DSpace]
          DSoftBreak -> [DHardBreak | inside == Broken]
          d -> maybe [d] (resolved inside) (unwrap d)

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
    ([b], ss)
      | Just xs <- traverse (\(v, s) -> (,) v <$> sole s) ss -> descend b xs
    (bs, ss) -> spliced bs ss
  where
    descend b xs = do
      (own, i) <- unwrap b
      others <- traverse (traverse unwrap) xs
      w <- joint (own : fmap (fst . snd) others)
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
        Just xs <- traverse (\c -> (,) (chVaried c) <$> sole (chWith c)) cs,
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

-- | The wrapper standing for several, if each is of a kind with the first:
-- the smallest span covering theirs for a region, for a group broken if any
-- is, and for a relation one to the referents of all of them.
joint :: [Wrapper] -> Maybe Wrapper
joint = \case
  w : ws | all (kin w) ws -> Just $ case w of
    WLocated s -> WLocated (foldr (<>) s [t | WLocated t <- ws])
    WFence s -> WFence (foldr (<>) s [t | WFence t <- ws])
    WGroup _
      | WGroup Broken `elem` (w : ws) -> WGroup Broken
      | WGroup Frangible `elem` (w : ws) -> WGroup Frangible
      | otherwise -> WGroup Flat
    WIndented n _ -> WIndented n (mconcat (nub [s | WIndented _ s <- w : ws]))
    _ -> w
  _ -> Nothing

-- | Could what these two wrappers hold be merged under one of them?
kin :: Wrapper -> Wrapper -> Bool
kin a b = case (a, b) of
  (WLocated s, WLocated t) -> meets s t
  (WFence s, WFence t) -> meets s t
  (WGroup _, WGroup _) -> True
  _ -> samePlacing a b

-- | Do two wrappers place what they hold alike, the referents they name
-- aside, which each configuration names by where it wrote them?
samePlacing ::
  -- | The first.
  Wrapper ->
  -- | The second.
  Wrapper ->
  Bool
samePlacing a b = case (a, b) of
  (WReferent _, WReferent _) -> True
  (WIndented n _, WIndented m _) -> n == m
  (WBracket x _, WBracket y _) -> x == y
  _ -> a == b

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
changesAgainst varied same bs xs =
  go 0 0 bs xs (lcs held (same `on` snd) bs' xs')
  where
    bs' = zip [0 ..] bs
    xs' = zip [0 ..] xs
    held = anchoring . snd

    go i _ b x [] = between i b x
    go i j b x (((p, _), (q, _)) : cs) =
      let (b', b'') = splitAt (p - i) b
          (x', x'') = splitAt (q - j) x
       in between i b' x' <> go (p + 1) (q + 1) (drop 1 b'') (drop 1 x'') cs

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
                guard
                  ( not (aligning w)
                      || beginsWithChoice y
                      || begins && firstOnItsLine y
                  )
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
          (\l -> T.all isSpace (T.take (offsetOf l (spanStartColumn s)) l))
          (lineAt (spanStartLine s) written)
      Nothing -> False

-- | The lines a run of documents prints and the space between them, each in
-- the wrappers it is printed in, and whether that cuts an element of the
-- run.
printedLines :: Layout -> [Doc] -> ([Doc], Bool)
printedLines layout xs = (plain . mconcat <$> filter (not . null) ps, cut)
  where
    (ps, _, cut) = partsOf layout Within xs

-- | A line less the wrappers that make no difference to it: a group around
-- what does not depend on the layout, and a region around regions and
-- nothing else, which say where the line was written already.
plain :: Doc -> Doc
plain = \case
  DGroup _ d | layoutFree d -> plain d
  DLocated _ d | onlyRegions d -> plain d
  DFence _ d | onlyRegions d -> plain d
  d -> mapChildren plain d
  where
    layoutFree = \case
      DBreak -> False
      DSoftBreak -> False
      DVariant{} -> False
      DCppChoice{} -> False
      DGroup{} -> True
      d -> getAll (foldChildren (All . layoutFree) d)
    onlyRegions d = isJust (regionOf d) && not (bare d)
    bare = \case
      DLocated{} -> False
      DFence{} -> False
      DText t -> not (T.null t)
      d -> getAny (foldChildren (Any . bare) d)

-- | Where a part of a run of documents begins.
data PartStart
  = -- | On a line, after what the line already holds.
    Within
  | -- | Between two lines.
    Between
  deriving (Eq)

-- | The parts a run of documents is cut into: its lines and the space
-- between them, each in the wrappers it is printed in.
partsOf ::
  -- | The layout the run is printed in.
  Layout ->
  -- | Where the run begins.
  PartStart ->
  -- | The run.
  [Doc] ->
  -- | The parts in order, the first of them what the run adds to the part
  -- it begins in, empty where the run begins a part of its own; where the
  -- run ends, which is where what follows it begins; and whether an element
  -- of the run holds what is printed on more than one line, so that the cut
  -- went through it.
  ([[Doc]], PartStart, Bool)
partsOf layout = walk [[]] False
  where
    walk ls cut place = \case
      [] -> (reverse ls, place, cut)
      x : xs
        | Just (w, y) <- unwrap x,
          not (aligning w) || place == Between,
          let inside = layoutInside layout w,
          (ps, place', cut') <- partsOf inside place (spineAt inside y),
          length (filter (not . null) ps) > 1 ->
            walk
              (onto (if aligning w then ps else wrapped w ps) ls)
              (cut || cut' || length (filter (not . all blank) ps) > 1)
              place'
              xs
        | place == Between,
          a : more <- xs,
          separatorSide a == Just LeadingSeparator,
          emptyRegion x ->
            walk ([x, a] : ls) cut Within more
        | printsNothing x || x == DSpace -> walk (onto [[x]] ls) cut place xs
        | opensWithBreak layout x,
          place == Between ->
            walk (onto [[x]] ls) cut place xs
        | opensWithBreak layout x -> walk ([x] : ls) cut Between xs
        | place == Within -> walk (onto [[x]] ls) cut place xs
        | otherwise -> walk ([x] : ls) cut Within xs
    onto ps ls = case (ps, ls) of
      (p : rest, l : done) -> reverse rest <> ((l <> p) : done)
      _ -> reverse ps <> ls
    wrapped w = case w of
      WLocated s -> marked DLocated s (startOf s) True
      WFence s -> marked DFence s (startOf s) True
      _ -> fmap (\p -> [wrap w (mconcat p) | not (null p)])
    marked region s from first = \case
      [] -> []
      p : rest
        | all blank p -> p : marked region s from first rest
        | Just r <- regionOf d -> p : marked region s (endOf r) False rest
        | otherwise -> [region s' d] : marked region s s' False rest
        where
          d = mconcat p
          final = all (all blank) rest
          s'
            | first && final = s
            | first = startOf s
            | final = endOf s
            | otherwise = from

-- | Where the regions a document holds were written.
writtenAt :: Doc -> Set Span
writtenAt = foldRegions (\s _ -> Set.insert s)

-- | A separator as it is written on the other side of the part it goes
-- with.
otherSide ::
  -- | The separator.
  Doc ->
  Doc
otherSide = \case
  DSeparator LeadingSeparator l t -> DSeparator TrailingSeparator l t
  DSeparator TrailingSeparator l t -> DSeparator LeadingSeparator l t
  DIndented n a d -> DIndented n a (otherSide d)
  DReferent k d -> DReferent k (otherSide d)
  d -> d

-- | A separator as it is written on this side of the part it goes with.
onSide ::
  -- | The side.
  SeparatorSide ->
  -- | The separator.
  Doc ->
  Doc
onSide side d
  | separatorSide d == Just side = d
  | otherwise = otherSide d

-- | Is this a region with nothing in it?
emptyRegion :: Doc -> Bool
emptyRegion = \case
  DLocated _ x -> printsNothing x
  _ -> False

-- | Does this document print nothing but space and what ends a line?
blank :: Doc -> Bool
blank = all spacing . spine
  where
    spacing x = onlySpacing x || maybe False (blank . snd) (unwrap x)

-- | Does the first thing this document puts on the page end a line?
opensWithBreak :: Layout -> Doc -> Bool
opensWithBreak layout d = case dropWhile (== DSpace) (spineAt layout d) of
  x : _
    | Just (w, y) <- unwrap x -> opensWithBreak (layoutInside layout w) y
    | otherwise -> case x of
        DHardBreak -> True
        DCloseLine -> True
        DBreak -> layout == Broken
        DSoftBreak -> layout == Broken
        DCppDirective _ _ -> True
        DCppChoice{} -> True
        _ -> False
  [] -> False

-- | Does a wrapper indent what it holds?
indenting ::
  -- | The wrapper.
  Wrapper ->
  Bool
indenting = \case
  WIndented{} -> True
  _ -> False

-- | Does a wrapper line what it holds up with the column it begins at?
aligning ::
  -- | The wrapper.
  Wrapper ->
  Bool
aligning = \case
  WAligned{} -> True
  _ -> False

-- | How much text this document holds, counting what a choice repeats once
-- for each alternative that repeats it.
weigh :: Layout -> Doc -> Int
weigh layout = go
  where
    go = \case
      Cat a b -> go a + go b
      DVariant flatD brokenD ->
        go (case layout of Broken -> brokenD; _ -> flatD)
      DCppChoice _ bs e -> sum (fmap (go . snd) bs) + go e
      DText t -> T.length t
      DCppDirective _ t -> T.length t
      d
        | Just (w, x) <- unwrap d -> weigh (layoutInside layout w) x
        | otherwise -> 0

-- | The longest run of elements two spines have in common, in order,
-- allowing for anything either of them has that the other does not, of
-- those that could hold them together, each with its counterpart.
lcs :: (a -> Bool) -> (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
lcs holds same xs ys =
  filter (holds . fst) (zip opening ys)
    <> table middleX middleY
    <> filter (holds . fst) closing
  where
    agreeing as bs = length (takeWhile id (zipWith same as bs))

    ahead = agreeing xs ys
    (opening, xs1) = splitAt ahead xs
    ys1 = drop ahead ys
    behind = agreeing (reverse xs1) (reverse ys1)
    (middleX, closingX) = splitAt (length xs1 - behind) xs1
    (middleY, closingY) = splitAt (length ys1 - behind) ys1
    closing = zip closingX closingY

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
                  | holds x, same x y -> cells (dn + 1) ((x, y) : ds) more
                  | n >= an -> cells n acc more
                  | otherwise -> cells an as' more

-- | Could this element hold two spines together, if it turned up in both?
anchoring ::
  -- | The element.
  Doc ->
  Bool
anchoring = \case
  DSeparator{} -> False
  DText "|" -> False
  DDeclarationsStart -> False
  DIndented _ a d | namesOnly a -> located d || not (blank d)
  DAligned _ d -> located d || not (blank d)
  DReferent _ d -> located d || not (blank d)
  DGroup _ d -> located d || not (blank d)
  d -> not (onlySpacing d)
  where
    located = \case
      DLocated{} -> True
      DFence{} -> True
      Cat a b -> located a || located b
      DIndented _ _ d -> located d
      DAligned _ d -> located d
      DReferent _ d -> located d
      DGroup _ d -> located d
      _ -> False

-- | A spine cut at the given positions: one stretch before each of them,
-- and one after the last.
cutAt :: [Int] -> [a] -> ([[a]], [a])
cutAt = go 0
  where
    go _ [] s = ([s], [])
    go i (p : ps) s = case splitAt (p - i) s of
      (before, x : after) ->
        let (stretches, xs) = go (p + 1) ps after
         in (before : stretches, x : xs)
      (before, []) -> ([before], [])
