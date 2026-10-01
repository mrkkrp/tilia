{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatting a module with the C preprocessor involved.
--
-- Each configuration is printed on its own and the documents are merged.
-- Working out what the configurations are is "Tilia.Cpp.Directives", whose
-- vocabulary this module re-exports so that callers need only one import.
module Tilia.Cpp
  ( -- * Formatting
    formatWithCpp,
    usesCpp,
    blankCpp,
    withoutRuledOut,
    CppError (..),
    describeCppError,

    -- * Splitting
    Guard (..),
    Configurations (..),
    configurations,
    leaves,
    branchLeaves,
    correspondingBranches,
    linearLeaves,
    countLeaves,
    answeredLeaves,
    answeredLinearLeaves,

    -- * Diagnostics
    regions,
  )
where

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put, runStateT)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.List (groupBy, maximumBy, sort, sortOn, transpose, unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Tilia.Cpp.Directives
import Tilia.Cpp.Place (CommentSummary, regionOf, restoreUnprinted, summarizeComments)
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Doc.Combinators qualified as Doc
import Tilia.Doc.Internal
  ( Conditional (..),
    Doc (..),
    Layout (..),
    conditionalRange,
    foldChildren,
    mapChildren,
    onlySpacing,
    printedFrom,
    printsNothing,
    spine,
    spineAt,
  )
import Tilia.Parser
  ( ParserConfig,
    parseConfiguration,
    pmSource,
  )
import Tilia.Render (RenderConfig (..), renderConfiguration)
import Tilia.Source
  ( Lines,
    Written (..),
    blankAt,
    comments,
    dropping,
    linesOf,
  )
import Tilia.Span
  ( Span,
    covers,
    meets,
    spanEndLine,
    spanStartLine,
  )

----------------------------------------------------------------------------
-- Formatting

-- | Format a module which uses the C preprocessor.
--
-- Each configuration's code is formatted by the ordinary printer, and the
-- resulting documents are merged. The conditionals, the comments, and the
-- directives are then injected into the merged document.
formatWithCpp ::
  -- | What to parse each configuration with.
  ParserConfig ->
  -- | What to print each configuration with.
  RenderConfig ->
  -- | The file this is, for the positions in an error.
  FilePath ->
  -- | The module, directives and all.
  Text ->
  -- | The formatted module, or why not.
  Either CppError Text
formatWithCpp parser render path source = do
  traverse_ (Left . RuledOutBranch) (ruledOutBranch source)
  (document, found) <-
    evalStateT
      (formatAllConfigs parser (knowing render) path (noAnswers source) source)
      configurationBudget
  formatted <- printDoc defaultRenderOptions <$> restoreUnprinted source found document
  formatted <$ traverse_ (Left . RuledOutBranch) (ruledOutBranch formatted)
  where
    knowing c =
      c{rcImportBarriers = maybe [] (sort . concatMap gsOwnLines . allGroups) (scanConditionals source)}

-- | Formatting that spends whole formattings out of the budget left, or
-- fails with a 'CppError'.
type Spending = StateT Int (Either CppError)

-- | Give up on formatting the module.
refuse :: CppError -> Spending a
refuse = lift . Left

-- | Format every configuration of a module, and merge them into one
-- document, with what they found besides their code.
formatAllConfigs ::
  -- | What to parse a configuration with.
  ParserConfig ->
  -- | What to print it with.
  RenderConfig ->
  -- | The file this is, for the positions in a parse error.
  FilePath ->
  -- | How this configuration was reached.
  Reached ->
  -- | Input text.
  Text ->
  Spending (Doc, CommentSummary)
formatAllConfigs parser render path reached source = do
  forest <- lift (readConditionals source)
  case variations forest source of
    Nothing -> do
      budget <- get
      when (budget <= 0) (refuse TooManyConfigurations)
      put (budget - 1)
      lift (formatSingleConfig parser render path reached (withoutOpaque source))
    Just apart -> do
      budget <- get
      when (linearCost budget forest source > budget) (refuse TooManyConfigurations)
      case runStateT (separately parser render path reached apart) budget of
        Left TooManyConfigurations -> refuse TooManyConfigurations
        Left _ -> jointly
        Right ((baseDoc, merged, found), left) -> do
          put left
          case combine Broken baseDoc (zip (fmap cfgWholes (vaGroups apart)) merged) of
            Just d -> pure (d, found)
            Nothing -> jointly
  where
    jointly
      | Right many <- countLeaves source,
        many > configurationsWorthTrying =
          refuse TooManyConfigurations
      | otherwise = case configurations source of
          Just c -> together parser render path reached c
          Nothing -> error "Tilia: a module that varies has a conditional to split on"

-- | How many formattings varying a module's conditionals one at a time
-- takes at the least, counted as far as one past the given number.
linearCost :: Int -> [GroupSpec] -> Text -> Int
linearCost limit forest source = go forest
  where
    written = linesOf (Written source)
    go = \case
      [] -> 1
      gss ->
        upTo
          0
          ( go (concatMap (`nestedIn` 0) gss)
              : [ if holdsNothing r && all holdsNothing (take 1 (gsBranches gs))
                    then 0
                    else go (nestedIn gs i <> concatMap (`nestedIn` 0) others)
                | (k, gs) <- zip [0 :: Int ..] gss,
                  let others = [o | (j, o) <- zip [0 ..] gss, j /= k],
                  (i, r) <-
                    zip
                      [1 ..]
                      ( drop 1 (gsBranches gs)
                          <> [ (1, 0)
                             | not (gsHasElse gs)
                             ]
                      )
                ]
          )
    holdsNothing (from, to) = all (`blankAt` written) [from .. to]
    upTo acc = \case
      _ | acc > limit -> acc
      [] -> acc
      x : xs -> upTo (acc + x) xs

-- | A single variation.
data Variation = Variation
  { -- | Every question answered with its first branch.
    vaBaseline :: Text,
    -- | The line ranges that answer leaves out.
    vaBaselineDropped :: [(Int, Int)],
    -- | One question varied, with all the others held at the baseline.
    vaGroups :: [Configurations]
  }

-- | Split a module on every conditional at its top level, one at a time.
variations :: [GroupSpec] -> Text -> Maybe Variation
variations forest source
  | null forest = Nothing
  | otherwise =
      Just
        Variation
          { vaBaseline = held (const 0),
            vaBaselineDropped = gone (const 0),
            vaGroups =
              [ Configurations
                  { cfgGuards = gsGuards gs,
                    cfgTexts = [held (varying k i) | i <- [0 .. gsCount gs - 1]],
                    cfgDropped = [gone (varying k i) | i <- [0 .. gsCount gs - 1]],
                    cfgWholes = Varied [gsWhole gs],
                    cfgDirectiveLines = [gsOwnLines gs]
                  }
              | (k, gs) <- zip [0 ..] forest
              ]
          }
  where
    held at = blanking (concat [blankingFor gs (at k) | (k, gs) <- zip [0 :: Int ..] forest]) source
    gone at = concat [droppedFor gs (at k) | (k, gs) <- zip [0 :: Int ..] forest]
    varying k i j = if j == k then i else 0

-- | Vary each conditional on its own, holding the others at their first
-- branch.
separately ::
  -- | What to parse a configuration with.
  ParserConfig ->
  -- | What to print it with.
  RenderConfig ->
  -- | The file this is, for the positions in a parse error.
  FilePath ->
  -- | How this configuration was reached.
  Reached ->
  -- | The conditionals to vary, and the baseline to hold them against.
  Variation ->
  Spending (Doc, [Doc], CommentSummary)
separately parser render path reached v = do
  (baseDoc, baseFound) <-
    formatAllConfigs
      parser
      render
      path
      (without (vaBaselineDropped v) reached)
      (vaBaseline v)
  groups <- for (vaGroups v) $ \c ->
    for (zip [0 ..] (cfgTexts c)) $ \(i, t) ->
      if t == vaBaseline v
        then pure (baseDoc, mempty)
        else formatAllConfigs parser render path (answering c i reached) t
  pure
    ( baseDoc,
      zipWith mergeOf (vaGroups v) (fmap (fmap fst) groups),
      baseFound <> foldMap (foldMap snd) groups
    )

-- | Vary the conditionals together, one group at a time.
together ::
  -- | What to parse a configuration with.
  ParserConfig ->
  -- | What to print it with.
  RenderConfig ->
  -- | The file this is, for the positions in a parse error.
  FilePath ->
  -- | How this configuration was reached.
  Reached ->
  -- | The group to split on, and the branch texts to split it into.
  Configurations ->
  Spending (Doc, CommentSummary)
together parser render path reached c = do
  formatted <- fmap catMaybes . for (zip [0 ..] (cfgTexts c)) $ \(i, t) ->
    if null (unconditionalErrors t)
      then Just . (,) i <$> formatAllConfigs parser render path (answering c i reached) t
      else pure Nothing
  docs <- lift (traverse (complete (fmap (fmap fst) formatted)) (zip [0 ..] (cfgTexts c)))
  pure (mergeOf c docs, foldMap (snd . snd) formatted)
  where
    complete formatted (i, t) = case lookup i formatted of
      Just d -> Right d
      Nothing -> case listToMaybe formatted >>= errorBranch (cfgWholes c) t . snd of
        Just d -> Right d
        Nothing ->
          Left
            . AbortingAlternative
            . maybe 0 dLine
            . listToMaybe
            . unconditionalErrors
            $ t

-- | Preserve an error-only alternative without asking the Haskell parser to
-- parse its missing expression or declaration. A successful sibling supplies
-- the surrounding syntax, with the nodes wholly inside the conditional taken
-- out; the @#error@ is put back into the space they leave with every other
-- directive. More complicated aborting alternatives are left unsupported.
errorBranch :: Varied -> Text -> Doc -> Maybe Doc
errorBranch (Varied ranges) source reference = foldl step (Just reference) ranges
  where
    sourceLines' = zip [1 ..] (T.lines source)
    errors = unconditionalErrors source
    step acc (from, to) = do
      doc <- acc
      let inside n = from <= n && n <= to
          here = filter (inside . dLine) errors
          errorLine n = any (\d -> dLine d <= n && n <= dLastLine d) here
          onlyErrors =
            all
              (\(n, l) -> not (inside n) || errorLine n || T.null (T.strip l))
              sourceLines'
          contained s = inside (spanStartLine s) && inside (spanEndLine s)
          outside d = case d of
            DLocated s _ | contained s -> mempty
            DCppChoice{}
              | lines'@(_ : _) <- printedFrom d,
                all (\(a, b) -> inside a && inside b) lines' ->
                  mempty
              | otherwise -> d
            _ -> mapChildren outside d
      if not (null here) && onlyErrors then Just (outside doc) else Nothing

-- | Format one configuration's code with the ordinary printer, and gather
-- what else it holds.
formatSingleConfig ::
  -- | What to parse it with.
  ParserConfig ->
  -- | What to print it with.
  RenderConfig ->
  -- | The file this is, for the positions in a parse error.
  FilePath ->
  -- | How this configuration was reached.
  Reached ->
  -- | The configuration itself, with no directives left in it.
  Text ->
  Either CppError (Doc, CommentSummary)
formatSingleConfig parser render path reached text =
  case parseConfiguration parser path (reachedLines reached) text of
    Left e -> Left (ConfigurationNotParsed (reachedAnswers reached) e)
    Right parsed ->
      let (document, loose) = renderConfiguration render parsed
       in Right (document, summarizeComments loose (comments (pmSource parsed)))

-- | How a configuration was reached, and what to call it.
data Reached = Reached
  { -- | Which branch each question was answered with, outermost first.
    reachedAnswers :: [([Guard], Int)],
    -- | Every line the author wrote, except for those written inside a
    -- branch this configuration did not take.
    reachedLines :: Lines
  }

-- | The configuration nothing has been decided about yet.
noAnswers :: Text -> Reached
noAnswers source =
  Reached
    { reachedAnswers = [],
      reachedLines = linesOf (Written source)
    }

-- | Answer one group's question with the branch at the given index.
answering :: Configurations -> Int -> Reached -> Reached
answering c i reached =
  reached
    { reachedAnswers = reachedAnswers reached <> [(cfgGuards c, i)],
      reachedLines =
        dropping
          (concat (take 1 (drop i (cfgDropped c))))
          (reachedLines reached)
    }

-- | Leave out the branches a baseline does not take, without answering
-- anything: the baseline is every question taken at its first branch, and
-- which question is being varied is not settled until 'answering'.
without :: [(Int, Int)] -> Reached -> Reached
without gone reached =
  reached{reachedLines = dropping gone (reachedLines reached)}

-- | How many whole formattings of a module one call may spend.
configurationBudget :: Int
configurationBudget = 64

-- | How many configurations a module may have and still be worth trying the
-- product on.
configurationsWorthTrying :: Integer
configurationsWorthTrying = 4096

-- | Merge the documents one group's configurations printed to.
mergeOf :: Configurations -> [Doc] -> Doc
mergeOf c =
  merge
    (fmap Conditional (cfgDirectiveLines c))
    (cfgGuards c)
    (cfgWholes c)

-- | Merge the documents one conditional's branches printed to.
--
-- A structural walk that keeps what they all agree on and puts a choice
-- where they part.
merge ::
  -- | The conditionals asking the question, as written.
  [Conditional] ->
  -- | The question.
  [Guard] ->
  -- | The lines its answer can change.
  Varied ->
  -- | One document per answer.
  [Doc] ->
  Doc
merge conditionals guards varied = go Broken
  where
    go _ [] = mempty
    go layout ds@(d : rest)
      | all (agree varied layout d) rest = d
      | Just xs <- traverse only spines = alongside layout xs
      | otherwise = factored layout spines
      where
        spines = fmap (spineAt layout) ds

    alongside _ [] = mempty
    alongside layout xs@(x : _) = case x of
      DLocated s _
        | Just tds <- every (\case DLocated t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst) tds ->
            case go layout (fmap snd tds) of
              DCppChoice{}
                | Just opened <- unwrapping layout (fmap fst tds) xs -> opened
              descended -> DLocated (hull s tds) descended
      DFence s _
        | Just tds <- every (\case DFence t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst) tds ->
            DFence (hull s tds) (go layout (fmap snd tds))
      DNest n _ | Just ds <- every (\case DNest m d | m == n -> Just d; _ -> Nothing) -> DNest n (go layout ds)
      DGroup _ _
        | Just lds <- every (\case DGroup l d -> Just (l, d); _ -> Nothing),
          ds@(d : rest) <- fmap snd lds ->
            let ls = [l | (l, inner) <- lds, not (printsNothing inner)]
                inside = if Broken `elem` ls then Broken else Flat
                merged = go inside ds
             in case merged of
                  DCppChoice{}
                    | not (all (== inside) ls),
                      not (all (agree varied inside d) rest) ->
                        choice xs
                  _ -> DGroup inside merged
      DAlign _ | Just ds <- every (\case DAlign d -> Just d; _ -> Nothing) -> DAlign (go layout ds)
      _
        | Just spans <- traverse regionOf xs,
          Just opened <- unwrapping layout spans xs ->
            opened
        | otherwise -> choice xs
      where
        every f = traverse f xs

    unwrapping layout spans xs = do
      inside <- sole [i | (i, s) <- zip [0 :: Int ..] spans, all (covers s) spans]
      wrapper <- listToMaybe (drop inside xs)
      if opens layout wrapper then Just (openedAgainst layout inside wrapper) else Nothing
      where
        sole [i] = Just i
        sole _ = Nothing

        openedAgainst l inside' d = case d of
          DLocated s x -> DLocated s (openedAgainst l inside' x)
          DFence s x -> DFence s (openedAgainst l inside' x)
          DNest n x -> DNest n (openedAgainst l inside' x)
          DAlign x -> DAlign (openedAgainst l inside' x)
          DGroup m x -> DGroup m (openedAgainst m inside' x)
          _ -> case spineAt l d of
            parts@(_ : _ : _) ->
              factored l [if k == inside' then parts else [e] | (k, e) <- zip [0 :: Int ..] xs]
            _ -> choice xs

    opens layout = \case
      DLocated _ x -> opens layout x
      DFence _ x -> opens layout x
      DNest _ x -> opens layout x
      DAlign x -> opens layout x
      DGroup l x -> opens l x
      d -> case spineAt layout d of
        _ : _ : _ -> True
        _ -> False

    factored layout ss =
      let same = agree varied layout
          shared = foldl1 (lcs same) ss
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
      | _ : _ : _ <- conditionals,
        Just owners <- traverse (traverse ownerOf) ss,
        present@(first' : _ : _) <- foldr insertOrdered [] [o | Just o <- concat owners],
        let assigned = fmap (settled first') owners,
        all ascending assigned =
          [ [[x | (x, o) <- zip xs os, o == i] | (xs, os) <- zip ss assigned]
          | i <- present
          ]
      | otherwise = [ss]
      where
        ranges = mapMaybe conditionalRange conditionals
        ownerOf x = case printedFrom x of
          [] -> Just Nothing
          lines' -> case sortOn fst [r | r@(from, to) <- ranges, all (\(a, b) -> from < a && b < to) lines'] of
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
       in (fmap reverse ss', reverse c)

    middle _ [] = mempty
    middle layout ss@(s : rest)
      | all (alike layout s) rest = mconcat s
      | Just xs <- traverse only ss = go layout xs
      | Just merged <- alongsideHeads layout ss,
        weigh layout merged < weigh layout apart =
          merged
      | otherwise = apart
      where
        apart = choice (fmap mconcat ss)

    alongsideHeads layout ss = do
      heads <- traverse listToMaybe ss
      let tails = fmap (drop 1) ss
      case heads of
        (h : hs)
          | all (sameKind h) hs,
            all breaksFirst tails ->
              Just (joined (go layout heads) (middle layout tails))
        _ -> Nothing
      where
        breaksFirst t = case dropWhile ((== 0) . weigh layout) t of
          [] -> True
          (d : _) -> opensWithBreak layout d

    joined before after = case (choiceAt Last before, choiceAt First after) of
      (Just (opening, ws, bs, e, gap), Just (gap', ws', cs, e', closing))
        | fmap fst bs == fmap fst cs,
          ws == ws' ->
            opening
              <> Doc.cppChoice
                ws
                [(g, x <> between <> y) | ((g, x), (_, y)) <- zip bs cs]
                (e <> between <> e')
              <> closing
        where
          between = gap <> gap'
      _ -> before <> after

    sameKind x y = case (x, y) of
      (DLocated s t, DLocated u v) -> meets s u && bothWritten t v
      (DFence s t, DFence u v) -> meets s u && bothWritten t v
      (DNest n t, DNest m v) -> n == m && bothWritten t v
      (DGroup _ t, DGroup _ v) -> bothWritten t v
      (DAlign t, DAlign v) -> bothWritten t v
      _ -> False
      where
        bothWritten t v = not (empty' t) && not (empty' v)
        empty' DEmpty = True
        empty' _ = False

    alike layout xs ys =
      length xs == length ys && and (zipWith (agree varied layout) xs ys)

    hoisted layout ss = case filter (not . null . middleOf) peeled of
      [] ->
        ( widest [l | (l, _, _) <- peeled],
          fmap (const []) ss,
          widest [r | (_, _, r) <- peeled]
        )
      speaking ->
        ( widest [l | (l, _, _) <- speaking],
          fmap middleOf peeled,
          widest [r | (_, _, r) <- speaking]
        )
      where
        peeled = fmap peel ss
        middleOf (_, m, _) = m
        widest = \case
          [] -> []
          runs -> maximumBy (comparing (spaceOf layout)) runs

    peel ds =
      let (l, rest) = span spacing ds
          (r, m) = span spacing (reverse rest)
       in (l, reverse m, reverse r)

    spacing = \case
      DEmpty -> True
      DBreak -> True
      DSoftBreak -> True
      DHardBreak -> True
      DCloseLine -> True
      _ -> False

    choice ds = case unsnoc ds of
      Just (branches, fallback) ->
        Doc.cppChoice
          (filter (evidenced ds) conditionals)
          (zip (fmap guardText guards) branches)
          fallback
      Nothing -> mempty

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
      (DGroup l x', DGroup m y') -> l == m && agree varied l x' y'
      (DNest n x', DNest m y') -> n == m && inside x' y'
      (DAlign x', DAlign y') -> inside x' y'
      (DLocated s x', DLocated t y') ->
        s == t && (untouched varied s || inside x' y')
      (DFence s x', DFence t y') -> s == t && inside x' y'
      (DCppChoice ws bs x', DCppChoice ws' cs y') ->
        ws == ws'
          && length bs == length cs
          && and [g == h && inside p q | ((g, p), (h, q)) <- zip bs cs]
          && inside x' y'
      (DText s, DText t) -> s == t
      (DCppDirective s u, DCppDirective t v) -> s == t && u == v
      (DHoldBack s, DHoldBack t) -> s == t
      (DVerbatimBreak r e, DVerbatimBreak q f) -> r == q && e == f
      (DSpace, DSpace) -> True
      (DBreak, DBreak) -> True
      (DSoftBreak, DSoftBreak) -> True
      (DHardBreak, DHardBreak) -> True
      (DCloseLine, DCloseLine) -> True
      _ -> False

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

    descend b xs = case b of
      DLocated s i
        | Just tds <- every (\case DLocated t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst . snd) tds ->
            DLocated (hull s (fmap snd tds)) <$> combine layout i (inner tds)
      DFence s i
        | Just tds <- every (\case DFence t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst . snd) tds ->
            DFence (hull s (fmap snd tds)) <$> combine layout i (inner tds)
      DNest n i | Just is <- every (\case DNest m d | m == n -> Just d; _ -> Nothing) -> DNest n <$> combine layout i is
      DAlign i | Just is <- every (\case DAlign d -> Just d; _ -> Nothing) -> DAlign <$> combine layout i is
      DGroup l i
        | Just ls <- every (\case DGroup m _ -> Just m; _ -> Nothing),
          Just is <- every (\case DGroup _ d -> Just d; _ -> Nothing) ->
            let inside = if Broken `elem` (l : fmap snd ls) then Broken else Flat
             in DGroup inside <$> combine inside i is
      _ -> Nothing
      where
        every f = traverse (\(v, d) -> (,) v <$> f d) xs
        inner tds = [(v, d) | (v, (_, d)) <- tds]

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

-- | The smallest span covering a node's own and those of everything merged
-- into it.
hull :: Span -> [(Span, Doc)] -> Span
hull = foldr ((<>) . fst)

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
changesAgainst varied same bs xs = go 0 bs xs (lcs same bs xs)
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
choiceAt :: Edge -> Doc -> Maybe (Doc, [Conditional], [(Text, Doc)], Doc, Doc)
choiceAt edge d = case span onlySpacing (inward (spine d)) of
  (outer, x : inner) ->
    let (before, after) = case edge of
          First -> (mconcat outer, mconcat inner)
          Last -> (mconcat (reverse inner), mconcat (reverse outer))
        around w (b, ws, bs, e, a) =
          (before <> w b, ws, fmap (fmap w) bs, w e, w a <> after)
     in case x of
          DCppChoice ws bs e -> Just (before, ws, bs, e, after)
          DGroup l y -> around (DGroup l) <$> choiceAt edge y
          DNest n y -> around (DNest n) <$> choiceAt edge y
          DLocated s y -> around (DLocated s) <$> choiceAt edge y
          DFence s y -> around (DFence s) <$> choiceAt edge y
          _ -> Nothing
  _ -> Nothing
  where
    inward = case edge of
      First -> id
      Last -> reverse

-- | Does the first thing this document puts on the page end a line?
opensWithBreak :: Layout -> Doc -> Bool
opensWithBreak layout d = case dropWhile quiet (spineAt layout d) of
  (x : _) -> case x of
    DNest _ y -> opensWithBreak layout y
    DAlign y -> opensWithBreak layout y
    DGroup l y -> opensWithBreak l y
    DLocated _ y -> opensWithBreak layout y
    DFence _ y -> opensWithBreak layout y
    DHardBreak -> True
    DCloseLine -> True
    DBreak -> layout == Broken
    DSoftBreak -> layout == Broken
    DCppDirective _ _ -> True
    DCppChoice{} -> True
    _ -> False
  [] -> False
  where
    quiet = \case
      DEmpty -> True
      DSpace -> True
      _ -> False

-- | How much text this document holds, counting what a choice repeats once
-- for each alternative that repeats it.
weigh :: Layout -> Doc -> Int
weigh layout = go
  where
    go = \case
      DCat a b -> go a + go b
      DNest _ d -> go d
      DAlign d -> go d
      DGroup l d -> weigh l d
      DVariant flatD brokenD ->
        go (case layout of Flat -> flatD; Broken -> brokenD)
      DLocated _ d -> go d
      DFence _ d -> go d
      DCppChoice _ bs e -> sum (fmap (go . snd) bs) + go e
      DText t -> T.length t
      DCppDirective _ t -> T.length t
      DHoldBack t -> T.length t
      _ -> 0

-- | The longest run of elements two spines have in common, in order,
-- allowing for anything either of them has that the other does not.
lcs :: (Doc -> Doc -> Bool) -> [Doc] -> [Doc] -> [Doc]
lcs same xs ys =
  filter anchoring opening <> table middleX middleY <> filter anchoring closing
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
                  | anchoring x, same x y -> cells (dn + 1) (x : ds) more
                  | n >= an -> cells n acc more
                  | otherwise -> cells an as' more

-- | Only let something that was printed line two spines up.
anchored :: (Doc -> Doc -> Bool) -> Doc -> Doc -> Bool
anchored same a b = anchoring a && same a b

-- | Could this element hold two spines together, if it turned up in both?
anchoring :: Doc -> Bool
anchoring = \case
  DEmpty -> False
  DSpace -> False
  DBreak -> False
  DSoftBreak -> False
  DHardBreak -> False
  DCloseLine -> False
  DVerbatimBreak _ _ -> False
  DNest _ d -> located d || not (printsNothing d)
  DAlign d -> located d || not (printsNothing d)
  DGroup _ d -> located d || not (printsNothing d)
  _ -> True
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
  (Doc -> Doc -> Bool) ->
  -- | The shared elements, in order, to cut at.
  [Doc] ->
  -- | The spine to cut.
  [Doc] ->
  -- | The stretches between the cuts, and the elements cut at.
  ([[Doc]], [Doc])
segments same = go
  where
    go [] s = ([s], [])
    go (c : cs) s = case break (same c) s of
      (before', matched : rest) -> keeping before' matched (go cs rest)
      (before', []) -> keeping before' c (go cs [])
      where
        keeping before' matched (stretches, anchors) =
          (before' : stretches, matched : anchors)

----------------------------------------------------------------------------
-- Diagnostics

-- | Every region a document records provenance for, with what was printed
-- there.
--
-- The outermost wins where a span appears twice, which is the one 'walk'
-- would have given a comment to.
regions :: Doc -> Map Span Doc
regions = Map.fromListWith (\_ outer -> outer) . go
  where
    go = \case
      DLocated s d -> (s, d) : go d
      DVariant a _ -> go a
      d -> foldChildren go d
