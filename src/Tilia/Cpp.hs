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

import Data.List (maximumBy, sortOn, transpose, unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Cpp.Directives
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Doc.Combinators qualified as Doc
import Tilia.Doc.Internal (Doc (..), Layout (..))
import Tilia.Parser
  ( ParserConfig,
    parseConfiguration,
  )
import Tilia.Render (RenderConfig (..), renderModule)
import Tilia.Source
  ( Lines,
    Written (..),
    directiveOnLine,
    dropping,
    lineTexts,
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
-- Each configuration is formatted by the ordinary printer. The resulting
-- documents are then merged.
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
formatWithCpp parser render path source =
  printDoc defaultRenderOptions . fst
    <$> formatAllConfigs
      parser
      (knowing render)
      path
      (noAnswers source)
      configurationBudget
      source
  where
    knowing c =
      c{rcImportBarriers = maybe [] (fmap dLine) (scanDirectives source)}

-- | Format every configuration of a module, and merge them into one
-- document.
formatAllConfigs ::
  -- | What to parse a configuration with.
  ParserConfig ->
  -- | What to print it with.
  RenderConfig ->
  -- | The file this is, for the positions in a parse error.
  FilePath ->
  -- | How this configuration was reached.
  Reached ->
  -- | Formattings left to spend.
  Int ->
  -- | Input text.
  Text ->
  Either CppError (Doc, Int)
formatAllConfigs parser render path reached budget source =
  case variations source of
    Nothing
      | any isDirective (T.lines left) ->
          Left (UnhandledDirective (unhandledIn left))
      | budget <= 0 -> Left TooManyConfigurations
      | otherwise -> do
          document <-
            formatSingleConfig
              parser
              render
              path
              reached
              left
          (,budget - 1)
            <$> replacing
              (reachedLines reached)
              (reachedAnswers reached)
              opaque
              document
      where
        opaque = opaqueDirectives source
        left = withoutOpaque source
    Just apart -> case linearly apart of
      Right built -> Right built
      Left (Refused TooManyConfigurations, _) ->
        Left TooManyConfigurations
      Left (_, left')
        | Right many <- countLeaves source,
          many > configurationsWorthTrying ->
            Left TooManyConfigurations
        | otherwise ->
            maybe
              (Left UnsplittableConditional)
              (together parser render path reached left')
              (configurations source)
  where
    linearly v =
      case separately parser render path reached budget v of
        Left why -> Left (Refused why, budget)
        Right (baseDoc, merged, budget') ->
          case combine Broken baseDoc (zip (fmap cfgWholes (vaGroups v)) merged) of
            Just d -> Right (d, budget')
            Nothing -> Left (InOneConstruct, budget')

-- | Why the linear form did not work.
data Linearly
  = -- | A configuration under it was refused, and this is what for.
    Refused CppError
  | -- | The merge came back a bare choice, so the conditionals' differences
    -- land on one construct and cannot be put back one at a time.
    InOneConstruct

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
variations :: Text -> Maybe Variation
variations source = do
  ds <- scanDirectives source
  specs <- traverse groupSpec (groupsAtLevel 0 ds)
  case [[gs] | gs <- specs] of
    [] -> Nothing
    dimensions ->
      let blanked at =
            concat
              [ blankingFor g (at k)
              | (k, dim) <-
                  zip [0 :: Int ..] dimensions,
                g <- dim
              ]
          gone at =
            concat
              [ droppedFor g (at k)
              | (k, dim) <-
                  zip [0 :: Int ..] dimensions,
                g <- dim
              ]
          held at = blanking (blanked at) source
       in Just
            Variation
              { vaBaseline = held (const 0),
                vaBaselineDropped = gone (const 0),
                vaGroups =
                  [ Configurations
                      { cfgGuards = gsGuards gs,
                        cfgTexts =
                          [ held (\j -> if j == k then i else 0)
                          | i <- [0 .. gsCount gs - 1]
                          ],
                        cfgDropped =
                          [ gone (\j -> if j == k then i else 0)
                          | i <- [0 .. gsCount gs - 1]
                          ],
                        cfgWholes = Varied (fmap gsWhole dim)
                      }
                  | (k, dim@(gs : _)) <- zip [0 :: Int ..] dimensions
                  ]
              }

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
  -- | Formattings left to spend.
  Int ->
  -- | The conditionals to vary, and the baseline to hold them against.
  Variation ->
  Either CppError (Doc, [Doc], Int)
separately parser render path reached budget v = do
  (baseDoc, spent) <-
    formatAllConfigs
      parser
      render
      path
      (without (vaBaselineDropped v) reached)
      budget
      (vaBaseline v)
  (merged, left) <- eachGroup baseDoc spent (vaGroups v)
  pure (baseDoc, merged, left)
  where
    free = freeOf reached

    eachGroup _ b [] = Right ([], b)
    eachGroup baseDoc b (c : cs) = do
      (docs, b') <- eachBranch c baseDoc b (zip [0 ..] (cfgTexts c))
      (rest, b'') <- eachGroup baseDoc b' cs
      pure (merge free (cfgGuards c) (cfgWholes c) docs : rest, b'')

    eachBranch _ _ b [] = Right ([], b)
    eachBranch c baseDoc b ((i, t) : ts) = do
      (d, b') <-
        if t == vaBaseline v
          then Right (baseDoc, b)
          else formatAllConfigs parser render path (answering c i reached) b t
      (ds, b'') <- eachBranch c baseDoc b' ts
      pure (d : ds, b'')

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
  -- | Formattings left to spend.
  Int ->
  -- | The group to split on, and the branch texts to split it into.
  Configurations ->
  Either CppError (Doc, Int)
together parser render path reached budget c = do
  (formatted, budget') <- eachBranch budget (zip [0 ..] (cfgTexts c))
  docs <- traverse (complete formatted) (zip [0 ..] (cfgTexts c))
  pure (merge (freeOf reached) (cfgGuards c) (cfgWholes c) docs, budget')
  where
    inside = reached
    eachBranch b [] = Right ([], b)
    eachBranch b ((_, t) : ts) | not (null (unconditionalErrors t)) = eachBranch b ts
    eachBranch b ((i, t) : ts) = do
      (d, b') <- formatAllConfigs parser render path (answering c i inside) b t
      (ds, b'') <- eachBranch b' ts
      pure ((i, d) : ds, b'')
    complete formatted (i, t) = case lookup i formatted of
      Just d -> Right d
      Nothing -> case listToMaybe formatted >>= errorBranch (cfgWholes c) t . snd of
        Just d -> Right d
        Nothing -> Left UnsplittableConditional

-- | Preserve an error-only alternative without asking the Haskell parser to
-- parse its missing expression or declaration. A successful sibling supplies
-- the surrounding syntax; only nodes wholly inside the conditional are
-- replaced. More complicated aborting alternatives are left unsupported.
errorBranch :: Varied -> Text -> Doc -> Maybe Doc
errorBranch (Varied ranges) source reference = foldl step (Just reference) ranges
  where
    sourceLines' = zip [1 ..] (T.lines source)
    errors = unconditionalErrors source
    step acc (from, to) = do
      doc <- acc
      let inside n = from <= n && n <= to
          here = filter (inside . opLine) errors
          errorLine n = any (\d -> opLine d <= n && n <= opLastLine d) here
          onlyErrors =
            all
              (\(n, l) -> not (inside n) || errorLine n || T.null (T.strip l))
              sourceLines'
          body = mconcat [DCppDirective (opSpan d) (opText d) | d <- here]
          contained s = inside (spanStartLine s) && inside (spanEndLine s)
          walk seen d = case d of
            DLocated s _ | contained s -> (True, if seen then mempty else body)
            DCppDirective s _ | contained s -> (True, if seen then mempty else body)
            DLocated s x -> fmap (DLocated s) (walk seen x)
            DFence s x -> fmap (DFence s) (walk seen x)
            DNest k x -> fmap (DNest k) (walk seen x)
            DAlign x -> fmap DAlign (walk seen x)
            DGroup l x -> fmap (DGroup l) (walk seen x)
            DVariant a b ->
              let (sa, a') = walk seen a; (sb, b') = walk seen b
               in (sa || sb, DVariant a' b')
            DCat a b ->
              let (sa, a') = walk seen a; (sb, b') = walk sa b
               in (sb, a' <> b')
            _ -> (seen, d)
          (placed, result) = walk False doc
      if not (null here) && onlyErrors && placed then Just result else Nothing

-- | Format one configuration with the ordinary printer.
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
  Either CppError Doc
formatSingleConfig parser render path reached text =
  case parseConfiguration parser path (reachedLines reached) text of
    Left e -> Left (ConfigurationNotParsed (reachedAnswers reached) e)
    Right parsed -> Right (renderModule render parsed)

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

-- | Put the directives that do not introduce new configurations back where
-- they were written.
replacing :: Lines -> [([Guard], Int)] -> [Opaque] -> Doc -> Either CppError Doc
replacing written answers opaque doc = foldl step (Right doc) opaque
  where
    step acc d
      | reproducedAt n doc = Left (DirectiveInQuotedText answers (keyword t))
      | otherwise =
          acc
            >>= maybe (Left (DirectiveUnplaceable answers (keyword t))) Right
              . place d
      where
        n = opLine d
        t = opText d
    keyword = T.takeWhile (/= ' ')
    reproducedAt n = any inside . located
      where
        inside (s, x) =
          spanStartLine s < n && n <= spanEndLine s && reproduced x

    located = \case
      DLocated s x -> (s, x) : located x
      DFence s x -> (s, x) : located x
      DNest _ x -> located x
      DAlign x -> located x
      DGroup _ x -> located x
      DVariant _ b -> located b
      DCat a b -> located a <> located b
      _ -> []

    reproduced = \case
      DVerbatimBreak _ _ -> True
      DNest _ x -> reproduced x
      DAlign x -> reproduced x
      DGroup _ x -> reproduced x
      DVariant _ b -> reproduced b
      DCat a b -> reproduced a || reproduced b
      _ -> False

    place directive = go
      where
        n = opLine directive

        body =
          DCppDirective (opSpan directive) (opText directive)
            <> if gapUnder written directive then Doc.blankLine else mempty

        go d = case d of
          DNest k x -> DNest k <$> go x
          DAlign x -> DAlign <$> go x
          DGroup l x -> DGroup l <$> go x
          DVariant a b -> DVariant <$> go a <*> go b
          DLocated s x | spanEndLine s >= n -> DLocated s <$> go x
          DFence s x | spanEndLine s >= n -> DFence s <$> go x
          DCat _ _ -> inSpine (spine d)
          _ -> Nothing

        inSpine parts = case break startsAfter parts of
          (before, after)
            | Just (earlier, holder, spacing) <- holding before,
              maybe False (>= n) (endOf holder) ->
                (\x -> mconcat (earlier <> [x] <> spacing <> after)) <$> go holder
            | Just (printed, anchor, spacing) <- tight before,
              Just from <- endOf anchor,
              not (gapWritten written (from + 1) (n - 1)) ->
                Just (mconcat (printed <> [anchor, body] <> spacing <> after))
            | otherwise -> Just (mconcat (before <> [body] <> after))
          where
            startsAfter x = maybe False (>= n) (startOf x)

        holding ds = case break (isJust . endOf) (reverse ds) of
          (spacing, holder : earlier) -> Just (reverse earlier, holder, reverse spacing)
          _ -> Nothing

        tight ds = case break (isJust . endOf) (reverse ds) of
          (spacing, anchor : earlier) -> Just (reverse earlier, anchor, reverse spacing)
          _ -> Nothing

    startOf = fmap fst . boundsOf
    endOf = fmap snd . boundsOf
    boundsOf = \case
      DLocated s _ -> Just (spanStartLine s, spanEndLine s)
      DFence s _ -> Just (spanStartLine s, spanEndLine s)
      DCppDirective s _ -> Just (spanStartLine s, spanEndLine s)
      DNest _ x -> boundsOf x
      DAlign x -> boundsOf x
      DGroup _ x -> boundsOf x
      DVariant _ b -> boundsOf b
      DCat a b -> case (boundsOf a, boundsOf b) of
        (Just (from, _), Just (_, to)) -> Just (from, to)
        (found, Nothing) -> found
        (Nothing, found) -> found
      _ -> Nothing

-- | Merge the documents one conditional's branches printed to.
--
-- A structural walk that keeps what they all agree on and puts a choice
-- where they part.
merge :: [(Span, Text)] -> [Guard] -> Varied -> [Doc] -> Doc
merge free guards varied = go Broken
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
              DCppChoice _ _
                | Just opened <- unwrapping layout (fmap fst tds) xs -> opened
              descended -> DLocated (hull s tds) descended
      DFence s _
        | Just tds <- every (\case DFence t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst) tds ->
            DFence (hull s tds) (go layout (fmap snd tds))
      DNest n _ | Just ds <- every (\case DNest m d | m == n -> Just d; _ -> Nothing) -> DNest n (go layout ds)
      DGroup _ _
        | Just ls <- every (\case DGroup l _ -> Just l; _ -> Nothing),
          Just ds@(d : rest) <- every (\case DGroup _ d -> Just d; _ -> Nothing) ->
            let inside = if Broken `elem` ls then Broken else Flat
                merged = go inside ds
             in case merged of
                  DCppChoice _ _
                    | not (all (== inside) ls),
                      not (all (agree varied inside d) rest) ->
                        choice xs
                  _ -> DGroup inside merged
      DAlign _ | Just ds <- every (\case DAlign d -> Just d; _ -> Nothing) -> DAlign (go layout ds)
      _ -> choice xs
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
      let exposed = fmap (exposing (filter split' (sharedDirectives ss))) ss
          split' d = d `elem` free && any (holds d) ss && not (all (holds d) ss)
          holds d = any (isNamed d)
          lining = alignable varied layout
          shared = foldl1 (lcs lining) exposed
          cut = fmap (segments (anchored lining) shared) exposed
          stretches = transpose (fmap fst cut)
          anchors = transpose (fmap snd cut)
       in mconcat (woven layout stretches (fmap (go layout) anchors))

    woven layout (s : ss) (c : cs) = varying layout s : c : woven layout ss cs
    woven layout ss [] = fmap (varying layout) ss
    woven _ [] _ = []

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

    joined before after = case (endingChoice before, startingChoice after) of
      (Just (opening, bs, e, gap), Just (gap', cs, e', closing))
        | fmap fst bs == fmap fst cs ->
            opening
              <> Doc.cppChoice
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
      Just (branches, fallback) -> Doc.cppChoice (zip (fmap guardText guards) branches) fallback
      Nothing -> mempty

    only [d] = Just d
    only _ = Nothing

-- | 'freeDirectives' of the module as its author wrote it.
freeOf :: Reached -> [(Span, Text)]
freeOf = freeDirectives . T.unlines . lineTexts . reachedLines

-- | The opaque directives written outside every conditional.
freeDirectives :: Text -> [(Span, Text)]
freeDirectives source =
  [ (opSpan d, opText d)
  | d <- opaqueDirectives source,
    Map.findWithDefault 0 (opLine d) depths == (0 :: Int)
  ]
  where
    depths = Map.fromList (zip [1 ..] (scanl step 0 (T.lines source)))
    step depth l = case directiveOnLine l of
      Just (keyword, _)
        | keyword `elem` ["if", "ifdef", "ifndef"] -> depth + 1
        | keyword == "endif" -> max 0 (depth - 1)
      _ -> depth

-- | Is this spine element the named directive itself, bare?
isNamed :: (Span, Text) -> Doc -> Bool
isNamed (s, t) = \case
  DCppDirective u v -> u == s && v == t
  _ -> False

-- | The directives every one of these spines holds.
sharedDirectives :: [[Doc]] -> [(Span, Text)]
sharedDirectives = \case
  [] -> []
  s : ss -> foldl (\acc t -> filter (`elem` namesIn t) acc) (namesIn s) ss
  where
    namesIn = concatMap named

-- | The directives a document holds, as far down as one may be brought out
-- from.
named :: Doc -> [(Span, Text)]
named = \case
  DCppDirective s t -> [(s, t)]
  DCat a b -> named a <> named b
  DNest _ x -> named x
  DAlign x -> named x
  DGroup _ x -> named x
  DVariant _ b -> named b
  _ -> []

-- | Bring the given directives out to the top of the spine.
exposing :: [(Span, Text)] -> [Doc] -> [Doc]
exposing wanted
  | null wanted = id
  | otherwise = concatMap out
  where
    out d
      | not (any here (named d)) = [d]
      | otherwise = case d of
          DCat a b -> out a <> out b
          DNest k x -> split (DNest k) (out x)
          DAlign x -> split DAlign (out x)
          DGroup l x -> split (DGroup l) (out x)
          DVariant a b -> varied (out a) (out b)
          _ -> [d]

    here (s, t) = (s, t) `elem` wanted

    bare = \case
      DCppDirective s t -> here (s, t)
      _ -> False

    split w ps = case break bare ps of
      (before, []) -> [w (mconcat before) | not (null before)]
      (before, x : rest) ->
        [w (mconcat before) | not (null before)] <> [x] <> split w rest

    varied as bs =
      let (xs, ds) = chunk as
          (ys, es) = chunk bs
       in if ds == es && length xs == length ys
            then interleave xs ys ds
            else [DVariant (mconcat as) (mconcat bs)]

    chunk ps = case break bare ps of
      (before, []) -> ([mconcat before], [])
      (before, x : rest) ->
        let (cs, ds) = chunk rest in (mconcat before : cs, x : ds)

    interleave (x : xs) (y : ys) ds = case ds of
      [] -> [DVariant x y]
      z : zs -> DVariant x y : z : interleave xs ys zs
    interleave _ _ _ = []

-- | Would these two documents print the same, laid out like this?
agree :: Varied -> Layout -> Doc -> Doc -> Bool
agree varied layout a b = alike (chunked (spineAt layout a)) (chunked (spineAt layout b))
  where
    alike (Left s : xs) (Left t : ys) = s == t && alike xs ys
    alike (Right x : xs) (Right y : ys) = here x y && alike xs ys
    alike [] [] = True
    alike _ _ = False
    chunked ds =
      let (space, rest) = span isSpace' ds
       in Left (spaceOf layout space) : case rest of
            [] -> []
            x : more -> Right x : chunked more
    isSpace' = \case
      DEmpty -> True
      DSpace -> True
      DBreak -> True
      DSoftBreak -> True
      DHardBreak -> True
      DCloseLine -> True
      _ -> False
    inside x y = agree varied layout x y
    here x y = case (x, y) of
      (DGroup l x', DGroup m y') -> l == m && agree varied l x' y'
      (DNest n x', DNest m y') -> n == m && inside x' y'
      (DAlign x', DAlign y') -> inside x' y'
      (DLocated s x', DLocated t y') ->
        s == t && (untouched varied s || inside x' y')
      (DFence s x', DFence t y') -> s == t && inside x' y'
      (DCppChoice bs x', DCppChoice cs y') ->
        length bs == length cs
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
                      [ changesAgainst v (alignable v layout) (agree v layout) bs s
                      | (v, s) <- ss
                      ]
                  )
              )
          )
      pure (mconcat (applied bs clustered))

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
  -- | Whether two elements stand for the same thing, which lines the spines up.
  (Doc -> Doc -> Bool) ->
  -- | Whether two elements print the same, which says nothing changed.
  (Doc -> Doc -> Bool) ->
  [Doc] ->
  [Doc] ->
  [Change]
changesAgainst varied lining plain bs xs = go 0 bs xs (lcs lining bs xs)
  where
    anchor = anchored lining

    go i b x [] = between i b x
    go i b x (c : cs) =
      let (b', b'') = break (anchor c) b
          (x', x'') = break (anchor c) x
          j = i + length b'
       in between i b' x'
            <> held j (listToMaybe b'') (listToMaybe x'')
            <> go (j + 1) (drop 1 b'') (drop 1 x'') cs

    held j (Just b') (Just x')
      | not (plain b' x') =
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
        shared = length (takeWhile id (zipWith plain b x))
        atEnd = length (takeWhile id (zipWith plain (reverse b) (reverse x)))
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

-- | What a document prints before its final choice, and that choice.
endingChoice :: Doc -> Maybe (Doc, [(Text, Doc)], Doc, Doc)
endingChoice d = case span onlySpacing (reverse (spine d)) of
  (trailing, x : earlier) ->
    let opening = mconcat (reverse earlier)
        gap = mconcat (reverse trailing)
        around w (o, bs, e, g) =
          (opening <> w o, fmap (fmap w) bs, w e, w g <> gap)
     in case x of
          DCppChoice bs e -> Just (opening, bs, e, gap)
          DGroup l y -> around (DGroup l) <$> endingChoice y
          DNest n y -> around (DNest n) <$> endingChoice y
          DLocated s y -> around (DLocated s) <$> endingChoice y
          DFence s y -> around (DFence s) <$> endingChoice y
          _ -> Nothing
  _ -> Nothing

-- | The mirror of 'endingChoice': a document's opening choice, and the rest.
startingChoice :: Doc -> Maybe (Doc, [(Text, Doc)], Doc, Doc)
startingChoice d = case span onlySpacing (spine d) of
  (leading, x : later) ->
    let gap = mconcat leading
        closing = mconcat later
        around w (g, bs, e, c) =
          (gap <> w g, fmap (fmap w) bs, w e, w c <> closing)
     in case x of
          DCppChoice bs e -> Just (gap, bs, e, closing)
          DGroup l y -> around (DGroup l) <$> startingChoice y
          DNest n y -> around (DNest n) <$> startingChoice y
          DLocated s y -> around (DLocated s) <$> startingChoice y
          DFence s y -> around (DFence s) <$> startingChoice y
          _ -> Nothing
  _ -> Nothing

-- | Nothing but the whitespace that separates one thing from the next.
onlySpacing :: Doc -> Bool
onlySpacing = \case
  DEmpty -> True
  DSpace -> True
  DBreak -> True
  DSoftBreak -> True
  DHardBreak -> True
  DCloseLine -> True
  _ -> False

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
    DCppChoice _ _ -> True
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
      DCppChoice bs e -> sum (fmap (go . snd) bs) + go e
      DText t -> T.length t
      DCppDirective _ t -> T.length t
      DHoldBack t -> T.length t
      _ -> 0

-- | A document as the sequence of things it concatenates.
spine :: Doc -> [Doc]
spine = \case
  DEmpty -> []
  DCat a b -> spine a <> spine b
  d -> [d]

-- | 'spine', with the variants resolved the way this layout will print them.
spineAt :: Layout -> Doc -> [Doc]
spineAt layout = go
  where
    go = \case
      DEmpty -> []
      DCat a b -> go a <> go b
      DVariant flatD brokenD ->
        go (case layout of Flat -> flatD; Broken -> brokenD)
      d -> [d]

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

-- | Do these two documents stand for the same thing?
alignable :: Varied -> Layout -> Doc -> Doc -> Bool
alignable = agree

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
  _ -> True

-- | A spine cut at the elements it shares with the others: one stretch
-- before each of them, and one after the last, and the elements themselves.
--
-- The matched elements come back rather than being dropped because the
-- caller cannot assume they are interchangeable: what lined them up is
-- 'alignable', and only 'agree' would say they print the same.
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
      DFence _ d -> go d
      DCat a b -> go a <> go b
      DNest _ d -> go d
      DAlign d -> go d
      DGroup _ d -> go d
      DVariant a _ -> go a
      DCppChoice bs e -> foldMap (go . snd) bs <> go e
      _ -> []
