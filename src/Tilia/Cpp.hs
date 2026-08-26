{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Formatting a module with the C preprocessor involved.
module Tilia.Cpp
  ( -- * Formatting
    formatWithCpp,
    usesCpp,
    blankCpp,
    CppError (..),
    describeCppError,

    -- * Splitting
    Guard (..),
    Configurations (..),
    configurations,
    leaves,
    linearLeaves,
    countLeaves,
    answeredLeaves,
    answeredLinearLeaves,

    -- * Diagnostics
    regions,
  )
where

import Data.Char (isAsciiLower)
import Data.List (sortOn, transpose, unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.LanguageExtensions.Type (Extension (..))
import Tilia.Comments (Above (..), Comment (..))
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Doc.Combinators qualified as Doc
import Tilia.Doc.Internal (Doc (..), Layout (..))
import Tilia.Parser
  ( ParseError,
    ParsedModule (..),
    ParserConfig,
    describeParseError,
    parseModule
  )
import Tilia.Render (RenderConfig (..), renderModule)
import Tilia.Span (Span, covers, meets, spanEndLine, spanStartLine)

----------------------------------------------------------------------------
-- Formatting

-- | Format a module which uses the C preprocessor.
--
-- Each configuration is formatted by the ordinary printer. The resulting
-- documents are then merged.
formatWithCpp ::
  -- | What to parse each configuration with
  ParserConfig ->
  -- | What to print each configuration with
  RenderConfig ->
  -- | The file this is, for the positions in an error
  FilePath ->
  -- | The module, directives and all
  Text ->
  -- | The formatted module, or why not
  Either CppError Text
formatWithCpp parser render path source =
  printDoc defaultRenderOptions . fst
    <$> formatAllConfigs
        parser
        (knowing render)
        path
        noAnswers
        configurationBudget
        source
  where
    knowing c =
      c {rcImportBarriers = maybe [] (map dLine) (scanDirectives source)}

-- | Format every configuration of a module, and merge them into one
-- document.
formatAllConfigs ::
  -- | What to parse a configuration with
  ParserConfig ->
  -- | What to print it with
  RenderConfig ->
  -- | The file this is, for the positions in a parse error
  FilePath ->
  -- | How this configuration was reached
  Reached ->
  -- | Formattings left to spend
  Int ->
  Text ->
  Either CppError (Doc, Int)
formatAllConfigs parser render path reached budget source = case variations source of
  Nothing
    | any isDirective (T.lines left) -> Left (UnhandledDirective (unhandledIn left))
    | budget <= 0 -> Left TooManyConfigurations
    | otherwise -> do
        document <-
          formatSingleConfig
            parser
            render
            path
            reached {reachedLines = reachedLines reached <> map opLine opaque}
            left
        (,budget - 1) <$> replacing (reachedAnswers reached) opaque document
    where
      opaque = opaqueDirectives source
      left = withoutOpaque source
  Just apart -> case linearly apart of
    Right built -> Right built
    Left (Refused TooManyConfigurations, _) -> Left TooManyConfigurations
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
      case separately parser render path (widen (vaDirectives v) reached) budget v of
        Left why -> Left (Refused why, budget)
        Right (baseDoc, merged, budget') ->
          case combine Broken baseDoc (zip (map cfgWholes (vaGroups v)) merged) of
            Just d -> Right (d, budget')
            Nothing -> Left (InOneConstruct, budget')

-- | Why the linear form did not work.
data Linearly
  = -- | A configuration under it was refused, and this is what for.
    Refused CppError
  | -- | The merge came back a bare choice, so the conditionals' differences
    -- land on one construct and cannot be put back one at a time.
    InOneConstruct

-- | What every question asked at the top level of a module splits it into,
-- each taken on its own.
data Variation = Variation
  { -- | Every question answered with its first branch.
    vaBaseline :: Text,
    -- | One question varied, with all the others held at the baseline.
    vaGroups :: [Configurations],
    -- | Every line any of their directives sat on.
    vaDirectives :: [Int]
  }

-- | Split a module on every conditional at its top level, one at a time.
variations :: Text -> Maybe Variation
variations source = do
  ds <- scanDirectives source
  specs <- traverse groupSpec (groupsAtLevel 0 ds)
  case [[gs] | gs <- specs] of
    [] -> Nothing
    dimensions ->
      let held at =
            blanking
              (concat [blankingFor g (at k) | (k, dim) <- zip [0 :: Int ..] dimensions, g <- dim])
              source
       in Just
            Variation
              { vaBaseline = held (const 0),
                vaGroups =
                  [ Configurations
                      { cfgGuards = gsGuards gs,
                        cfgTexts =
                          [ held (\j -> if j == k then i else 0)
                            | i <- [0 .. gsCount gs - 1]
                          ],
                        cfgDirectives = concatMap gsOwnLines dim,
                        cfgWholes = Varied (map gsWhole dim)
                      }
                    | (k, dim@(gs : _)) <- zip [0 :: Int ..] dimensions
                  ],
                vaDirectives = concatMap gsOwnLines (concat dimensions)
              }

-- | Vary each conditional on its own, holding the others at their first
-- branch.
separately ::
  -- | What to parse a configuration with
  ParserConfig ->
  -- | What to print it with
  RenderConfig ->
  -- | The file this is, for the positions in a parse error
  FilePath ->
  -- | How this configuration was reached
  Reached ->
  -- | Formattings left to spend
  Int ->
  -- | The conditionals to vary, and the baseline to hold them against
  Variation ->
  Either CppError (Doc, [Doc], Int)
separately parser render path reached budget v = do
  (baseDoc, spent) <- formatAllConfigs parser render path reached budget (vaBaseline v)
  (merged, left) <- eachGroup baseDoc spent (vaGroups v)
  pure (baseDoc, merged, left)
  where
    eachGroup _ b [] = Right ([], b)
    eachGroup baseDoc b (c : cs) = do
      (docs, b') <- eachBranch c baseDoc b (zip [0 ..] (cfgTexts c))
      (rest, b'') <- eachGroup baseDoc b' cs
      pure (merge (cfgGuards c) (cfgWholes c) docs : rest, b'')

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
  -- | What to parse a configuration with
  ParserConfig ->
  -- | What to print it with
  RenderConfig ->
  -- | The file this is, for the positions in a parse error
  FilePath ->
  -- | How this configuration was reached
  Reached ->
  -- | Formattings left to spend
  Int ->
  -- | The group to split on, and the branch texts to split it into
  Configurations ->
  Either CppError (Doc, Int)
together parser render path reached budget c = do
  (docs, budget') <- eachBranch budget (zip [0 ..] (cfgTexts c))
  pure (merge (cfgGuards c) (cfgWholes c) docs, budget')
  where
    inside = widen (cfgDirectives c) reached
    eachBranch b [] = Right ([], b)
    eachBranch b ((i, t) : ts) = do
      (d, b') <- formatAllConfigs parser render path (answering c i inside) b t
      (ds, b'') <- eachBranch b' ts
      pure (d : ds, b'')

-- | Format one configuration with the ordinary printer.
formatSingleConfig ::
  -- | What to parse it with
  ParserConfig ->
  -- | What to print it with
  RenderConfig ->
  -- | The file this is, for the positions in a parse error
  FilePath ->
  -- | How this configuration was reached
  Reached ->
  -- | The configuration itself, with no directives left in it
  Text ->
  Either CppError Doc
formatSingleConfig parser render path reached text =
  case parseModule parser path text of
    Left e -> Left (ConfigurationNotParsed (reachedAnswers reached) e)
    Right parsed ->
      Right (renderModule render (truthfully (reachedLines reached) parsed))

-- | Put back what blanking took away.
truthfully :: [Int] -> ParsedModule -> ParsedModule
truthfully directiveLines parsed =
  parsed {pmComments = map correct (pmComments parsed)}
  where
    correct c
      | above `elem` directiveLines =
          c {commentAbove = ContentAt 1, commentGapAbove = False}
      | otherwise = c
      where
        above = spanStartLine (commentSpan c) - 1

-- | How a configuration was reached, and what to call it.
data Reached = Reached
  { -- | Directive lines blanked by the calls above this one. See
    -- 'truthfully', which is what needs them.
    reachedLines :: [Int],
    -- | Which branch each question was answered with, outermost first.
    reachedAnswers :: [([Guard], Int)]
  }

-- | The configuration nothing has been decided about yet.
noAnswers :: Reached
noAnswers = Reached {reachedLines = [], reachedAnswers = []}

-- | Blank more directive lines on the way into a group.
widen :: [Int] -> Reached -> Reached
widen ls reached = reached {reachedLines = reachedLines reached <> ls}

-- | Answer one group's question with the branch at the given index.
answering :: Configurations -> Int -> Reached -> Reached
answering c i reached =
  reached {reachedAnswers = reachedAnswers reached <> [(cfgGuards c, i)]}

-- | How many whole formattings of a module one call may spend.
configurationBudget :: Int
configurationBudget = 64

-- | How many configurations a module may have and still be worth trying the
-- product on.
configurationsWorthTrying :: Integer
configurationsWorthTrying = 4096

-- | Put the directives that do not introduce new configurations back where
-- they were written.
replacing :: [([Guard], Int)] -> [Opaque] -> Doc -> Either CppError Doc
replacing answers opaque doc = foldl step (Right doc) opaque
  where
    step acc (Opaque n t gap)
      | reproducedAt n doc = Left (DirectiveInQuotedText answers (keyword t))
      | otherwise =
          acc >>= maybe (Left (DirectiveUnplaceable answers (keyword t))) Right
            . place n written
      where
        written = DCppDirective t <> if gap then Doc.blankLine else mempty
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
      DVerbatimBreak _ -> True
      DNest _ x -> reproduced x
      DAlign x -> reproduced x
      DGroup _ x -> reproduced x
      DVariant _ b -> reproduced b
      DCat a b -> reproduced a || reproduced b
      _ -> False

    place n written = go
      where
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
            | otherwise -> Just (mconcat (before <> [written] <> after))
          where
            startsAfter x = maybe False (>= n) (startOf x)

        holding ds = case break (isJust . endOf) (reverse ds) of
          (spacing, holder : earlier) -> Just (reverse earlier, holder, reverse spacing)
          _ -> Nothing

    startOf = fmap fst . boundsOf

    endOf = fmap snd . boundsOf

    boundsOf = \case
      DLocated s _ -> Just (spanStartLine s, spanEndLine s)
      DFence s _ -> Just (spanStartLine s, spanEndLine s)
      DNest _ x -> boundsOf x
      DAlign x -> boundsOf x
      DGroup _ x -> boundsOf x
      DVariant _ b -> boundsOf b
      DCat a b -> case (boundsOf a, boundsOf b) of
        (Just (from, _), Just (_, to)) -> Just (from, to)
        (found, Nothing) -> found
        (Nothing, found) -> found
      _ -> Nothing

-- | A module with the directives that ask nothing blanked out of it.
--
-- The same blanking every branch gets, and for the same reason: what is
-- left occupies the lines it always did, so everything downstream can go on
-- lining documents up by where they came from.
withoutOpaque :: Text -> Text
withoutOpaque source =
  blanking [(opLine d, opLine d) | d <- opaqueDirectives source] source

-- | Merge the documents one conditional's branches printed to.
--
-- A structural walk that keeps what they all agree on and puts a choice
-- where they part.
merge :: [Guard] -> Varied -> [Doc] -> Doc
merge guards varied = go Broken
  where
    go _ [] = mempty
    go layout ds@(d : rest)
      | all (agree varied layout d) rest = d
      | Just xs <- traverse only spines = alongside layout xs
      | otherwise = factored layout spines
      where
        spines = map spine ds

    alongside _ [] = mempty
    alongside layout xs@(x : _) = case x of

      DLocated s _
        | Just tds <- every (\case DLocated t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst) tds ->
            case go layout (map snd tds) of
              DCppChoice _ _
                | Just opened <- unwrapping layout (map fst tds) xs -> opened
              descended -> DLocated (hull s tds) descended
      DFence s _
        | Just tds <- every (\case DFence t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst) tds ->
            DFence (hull s tds) (go layout (map snd tds))
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
      DVariant _ _
        | Just as <- every (\case DVariant a _ -> Just a; _ -> Nothing),
          Just bs <- every (\case DVariant _ b -> Just b; _ -> Nothing) ->
            DVariant (go Flat as) (go Broken bs)
      _ -> choice xs
      where
        every f = traverse f xs

    unwrapping layout spans xs = do
      inside <- sole [i | (i, s) <- zip [0 :: Int ..] spans, all (covers s) spans]
      wrapper <- listToMaybe (drop inside xs)
      if opens wrapper then Just (openedAgainst layout inside wrapper) else Nothing
      where
        sole [i] = Just i
        sole _ = Nothing

        openedAgainst l inside' d = case d of
          DLocated s x -> DLocated s (openedAgainst l inside' x)
          DFence s x -> DFence s (openedAgainst l inside' x)
          DNest n x -> DNest n (openedAgainst l inside' x)
          DAlign x -> DAlign (openedAgainst l inside' x)
          DGroup m x -> DGroup m (openedAgainst m inside' x)
          DVariant a b ->
            DVariant (openedAgainst Flat inside' a) (openedAgainst Broken inside' b)
          _ -> case spine d of
            parts@(_ : _ : _) ->
              factored l [if k == inside' then parts else [e] | (k, e) <- zip [0 :: Int ..] xs]
            _ -> choice xs

    opens = \case
      DLocated _ x -> opens x
      DFence _ x -> opens x
      DNest _ x -> opens x
      DAlign x -> opens x
      DGroup _ x -> opens x
      DVariant _ b -> opens b
      d -> case spine d of
        _ : _ : _ -> True
        _ -> False

    factored layout ss =
      let plain = agree varied layout
          same = anchored plain
          shared = foldl1 (lcs plain) ss
          stretches = transpose (map (segments same shared) ss)
       in mconcat (woven layout stretches shared)

    woven layout (s : ss) (c : cs) = varying layout s : c : woven layout ss cs
    woven layout ss [] = map (varying layout) ss
    woven _ [] _ = []

    varying layout ss =
      let (opening, ss1) = sharedStart layout ss
          (ss2, closing) = sharedEnd layout ss1
          (lead, ss3, trail) = hoisted ss2
       in mconcat opening
            <> mconcat lead
            <> middle layout ss3
            <> mconcat trail
            <> mconcat closing

    sharedStart layout ss
      | Just (h : hs) <- traverse listToMaybe ss,
        all (agree varied layout h) hs =
          let (c, ss') = sharedStart layout (map (drop 1) ss) in (h : c, ss')
      | otherwise = ([], ss)

    sharedEnd layout ss =
      let (c, ss') = sharedStart layout (map reverse ss)
       in (map reverse ss', reverse c)

    middle _ [] = mempty
    middle layout ss@(s : rest)
      | all (alike layout s) rest = mconcat s
      | Just xs <- traverse only ss = go layout xs
      | otherwise = choice (map mconcat ss)

    alike layout xs ys =
      length xs == length ys && and (zipWith (agree varied layout) xs ys)

    hoisted ss = case map peel (filter (not . null) ss) of
      peeled@((lead, _, trail) : _)
        | all (\(l, _, r) -> l == lead && r == trail) peeled ->
            (lead, map trimmed ss, trail)
      _ -> ([], ss, [])
      where
        trimmed s = if null s then [] else let (_, m, _) = peel s in m

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
      Just (branches, fallback) -> Doc.cppChoice (zip (map guardText guards) branches) fallback
      Nothing -> mempty

    only [d] = Just d
    only _ = Nothing

-- | Would these two documents print the same, laid out like this?
agree :: Varied -> Layout -> Doc -> Doc -> Bool
agree varied layout a b = alike (spine a) (spine b)
  where
    alike [] [] = True
    alike (x : xs) (y : ys) = here x y && alike xs ys
    alike _ _ = False

    inside x y = agree varied layout x y

    here x y = case (x, y) of
      (DVariant flatX brokenX, DVariant flatY brokenY) -> case layout of
        Flat -> agree varied Flat flatX flatY
        Broken -> agree varied Broken brokenX brokenY
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
      (DCppDirective s, DCppDirective t) -> s == t
      (DHoldBack s, DHoldBack t) -> s == t
      (DVerbatimBreak r, DVerbatimBreak q) -> r == q
      (DSpace, DSpace) -> True
      (DBreak, DBreak) -> True
      (DSoftBreak, DSoftBreak) -> True
      (DHardBreak, DHardBreak) -> True
      (DCloseLine, DCloseLine) -> True
      _ -> False

-- | The lines one conditional could have printed differently.
newtype Varied = Varied {variedLines :: [(Int, Int)]}
  deriving (Eq, Show)

-- | Was this region printed from lines the conditional left alone?
untouched :: Varied -> Span -> Bool
untouched (Varied ranges) s = not (any reaches ranges)
  where
    reaches (from, to) = spanStartLine s <= to && from <= spanEndLine s

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
  many -> case (spine base, [(v, spine d) | (v, d) <- many]) of
    ([b], ss) | Just xs <- traverse (\(v, s) -> (,) v <$> single s) ss -> descend b xs
    (bs, ss) -> spliced bs ss
  where
    single [d] = Just d
    single _ = Nothing

    descend b xs = case b of
      DLocated s i
        | Just tds <- every (\case DLocated t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst . snd) tds ->
            DLocated (hull s (map snd tds)) <$> combine layout i (inner tds)
      DFence s i
        | Just tds <- every (\case DFence t d -> Just (t, d); _ -> Nothing),
          all (meets s . fst . snd) tds ->
            DFence (hull s (map snd tds)) <$> combine layout i (inner tds)
      DNest n i | Just is <- every (\case DNest m d | m == n -> Just d; _ -> Nothing) -> DNest n <$> combine layout i is
      DAlign i | Just is <- every (\case DAlign d -> Just d; _ -> Nothing) -> DAlign <$> combine layout i is
      DGroup l i
        | Just ls <- every (\case DGroup m _ -> Just m; _ -> Nothing),
          Just is <- every (\case DGroup _ d -> Just d; _ -> Nothing) ->
            let inside = if Broken `elem` (l : map snd ls) then Broken else Flat
             in DGroup inside <$> combine inside i is
      DVariant i j
        | Just is <- every (\case DVariant a _ -> Just a; _ -> Nothing),
          Just js <- every (\case DVariant _ b' -> Just b'; _ -> Nothing) ->
            case layout of
              Flat -> (`DVariant` j) <$> combine Flat i is
              Broken -> DVariant i <$> combine Broken j js
      _ -> Nothing
      where
        every f = traverse (\(v, d) -> (,) v <$> f d) xs
        inner tds = [(v, d) | (v, (_, d)) <- tds]

    spliced bs ss = do
      clustered <-
        traverse
          (cluster bs)
          ( overlapping
              (sortOn chFrom (concat [changesAgainst v (agree v layout) bs s | (v, s) <- ss]))
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
        from = minimum (map chFrom cs)
        to = maximum (map chTo cs)

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
  { chFrom :: !Int,
    chTo :: !Int,
    chWith :: [Doc],
    -- | The lines the conditional this change came from could have reached.
    -- Carried so that a cluster of two of them can be combined without
    -- losing which conditional each half belongs to. See 'Varied'.
    chVaried :: Varied
  }

-- | What one document changed about the baseline, as the stretches it
-- replaced and what it put in each of their places.
changesAgainst :: Varied -> (Doc -> Doc -> Bool) -> [Doc] -> [Doc] -> [Change]
changesAgainst varied same bs xs = go 0 bs xs (lcs same bs xs)
  where
    anchor = anchored same

    go i b x [] = between i b x
    go i b x (c : cs) =
      let (b', b'') = break (anchor c) b
          (x', x'') = break (anchor c) x
       in between i b' x'
            <> go (i + length b' + 1) (drop 1 b'') (drop 1 x'') cs

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

-- | A document as the sequence of things it concatenates.
spine :: Doc -> [Doc]
spine = \case
  DEmpty -> []
  DCat a b -> spine a <> spine b
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
            cells !n acc rest = (n, acc) : case rest of
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
  DVerbatimBreak _ -> False
  _ -> True

-- | A spine cut at the elements it shares with the others: one stretch
-- before each of them, and one after the last.
--
-- Always one more stretch than there are shared elements, either end of
-- which may be empty. Leftmost matching is enough to find each of them,
-- since what is being matched is a subsequence of this spine to begin with.
segments :: (Doc -> Doc -> Bool) -> [Doc] -> [Doc] -> [[Doc]]
segments same = go
  where
    go [] s = [s]
    go (c : cs) s =
      let (before', rest) = break (same c) s
       in before' : go cs (drop 1 rest)

-- | Would the preprocessor be run over this module, and find anything to
-- do?
usesCpp :: [Extension] -> Text -> Bool
usesCpp extensions source =
  Cpp `elem` extensions && any isDirective (T.lines source)

-- | Blank out the directive lines, keeping every branch.
blankCpp :: Text -> Text
blankCpp = T.unlines . map blank . T.lines
  where
    blank l = if isDirective l then "" else l

-- | Why a module using the preprocessor could not be formatted.
data CppError
  = -- | A directive we do not handle, and its keyword.
    UnhandledDirective Text
  | -- | Conditionals that do not nest, or an @#else@ out of place.
    UnsplittableConditional
  | -- | More configurations than 'configurationBudget' allows.
    TooManyConfigurations
  | -- | A configuration the parser rejected, and which one it was.
    ConfigurationNotParsed [([Guard], Int)] ParseError
  | -- | A directive whose place in the document could not be found.
    DirectiveUnplaceable [([Guard], Int)] Text
  | -- | A directive written inside a quasiquote or other verbatim text.
    DirectiveInQuotedText [([Guard], Int)] Text

-- | Say what went wrong, in one line. The edge of the system.
describeCppError :: CppError -> Text
describeCppError = \case
  UnhandledDirective k -> "a #" <> k <> " directive, which we do not handle"
  UnsplittableConditional -> "a conditional this prototype cannot split"
  TooManyConfigurations -> "too many configurations to format"
  ConfigurationNotParsed c e -> describeParseError e <> inConfiguration c
  DirectiveUnplaceable c k -> "nowhere to put the #" <> k <> inConfiguration c
  DirectiveInQuotedText c k ->
    "a #" <> k <> " inside something quoted verbatim" <> inConfiguration c

-- | Which configuration, in words. Empty where there is only one.
inConfiguration :: [([Guard], Int)] -> Text
inConfiguration [] = ""
inConfiguration answers =
  ", in the configuration taking " <> T.intercalate ", then " (map said answers)
  where
    said (guards, i) = case (drop i guards, guards) of
      (g : _, _) -> "#" <> guardText g
      ([], g : _) -> "no branch of #" <> guardText g
      ([], []) -> "no branch"

-- | The keyword of the first directive here that we do not handle.
unhandledIn :: Text -> Text
unhandledIn source =
  case [keywordOf l | l <- T.lines source, isDirective l] of
    k : _ -> k
    [] -> ""
  where
    keywordOf = T.takeWhile isAsciiLower . T.stripStart . T.drop 1 . T.stripStart

----------------------------------------------------------------------------
-- Splitting

-- | One conditional directive, as written after its hash.
newtype Guard = Guard {guardText :: Text}
  deriving (Eq, Ord, Show)

-- | What one conditional splits a module into.
data Configurations = Configurations
  { -- | The directives: the @#if@ of the group, and then one per @#elif@.
    cfgGuards :: [Guard],
    -- | One module text per branch, in the same order as the directives, and
    -- then one more for the @#else@.
    cfgTexts :: [Text],
    -- | The lines this group's own directives were on.
    --
    -- Blanking them keeps the line numbers, which is what the whole design
    -- rests on, but it also tells the comment machinery that those lines
    -- were empty. They were not, and a comment written directly under a
    -- directive would otherwise be printed with a blank line above it that
    -- its author never wrote. See 'truthfully'.
    cfgDirectives :: [Int],
    -- | From each tied group's @#if@ to its @#endif@, inclusive.
    --
    -- Everything a branch of this conditional can be responsible for lies
    -- between one of these pairs, because that is what a group /is/. What
    -- reads them is 'Varied', and what it does with them is skip the rest of
    -- the module.
    cfgWholes :: Varied
  }
  deriving (Eq, Show)

-- | Split a module on its first outermost conditional, and on every other one
-- written behind the same directives, wherever in the module it sits.
configurations :: Text -> Maybe Configurations
configurations source = do
  ds <- scanDirectives source
  gs <- groupSpec =<< listToMaybe (groupsAtLevel 0 ds)
  let tied = sameGuard gs ds
  pure
    Configurations
      { cfgGuards = gsGuards gs,
        cfgTexts =
          [ blanking (concatMap (`blankingFor` i) tied) source
          | i <- [0 .. gsCount gs - 1]
          ],
        cfgDirectives = concatMap gsOwnLines tied,
        cfgWholes = Varied (map gsWhole tied)
      }

-- | One conditional group, read off the directives that make it up.
data GroupSpec = GroupSpec
  { gsGuards :: [Guard],
    gsHasElse :: Bool,
    gsOwnLines :: [Int],
    gsBranches :: [(Int, Int)],
    gsWhole :: (Int, Int)
  }

-- | Read a group off its directives, refusing one that is malformed.
groupSpec :: [Directive] -> Maybe GroupSpec
groupSpec group = do
  (separators, end) <- unsnoc group
  opener <- listToMaybe separators
  require (dKeyword opener `elem` opensGroup)
  require (dKeyword end == "endif")
  require (all ((`elem` continuesGroup) . dKeyword) (drop 1 separators))
  require (all ((/= "else") . dKeyword) (drop 1 (reverse separators)))
  pure
    GroupSpec
      { gsGuards = [dGuard d | d <- separators, dKeyword d /= "else"],
        gsHasElse = any ((== "else") . dKeyword) separators,
        gsOwnLines = map dLine group,
        gsBranches = [(dLine a + 1, dLine b - 1) | (a, b) <- zip group (drop 1 group)],
        gsWhole = (dLine opener, dLine end)
      }
  where
    require b = if b then Just () else Nothing

-- | How many configurations a group has: one per condition, and one more
-- for when none of them holds.
gsCount :: GroupSpec -> Int
gsCount gs = length (gsGuards gs) + 1

-- | The lines to blank so that configuration @i@ of a group is what is left.
blankingFor :: GroupSpec -> Int -> [(Int, Int)]
blankingFor gs i
  | i < length (gsGuards gs) || gsHasElse gs =
      [(n, n) | n <- gsOwnLines gs]
        <> [r | (k, r) <- zip [0 :: Int ..] (gsBranches gs), k /= i]
  | otherwise = [gsWhole gs]

-- | Every conditional in a module, at whatever depth it sits.
allGroups :: [Directive] -> [[Directive]]
allGroups ds = concat [groupsAtLevel l ds | l <- [0 .. deepest]]
  where
    deepest = maximum (0 : map dLevel ds)

-- | Every group in a module written behind the same directives as this one.
sameGuard :: GroupSpec -> [Directive] -> [GroupSpec]
sameGuard gs ds =
  [g | grp <- allGroups ds, Just g <- [groupSpec grp], gsGuards g == gsGuards gs]

-- | The directives at one level of nesting, split into the groups they make
-- up.
groupsAtLevel :: Int -> [Directive] -> [[Directive]]
groupsAtLevel level = split . filter ((== level) . dLevel)
  where
    split ds = case break ((== "endif") . dKeyword) ds of
      (_, []) -> []
      (before', end : rest) -> (before' <> [end]) : split rest

-- | One preprocessor directive, and how deep in the conditionals it sits.
data Directive = Directive
  { dLine :: !Int,
    dKeyword :: !Text,
    dGuard :: !Guard,
    dLevel :: !Int
  }
  deriving (Eq, Show)

-- | Every conditional directive in a module, or 'Nothing' if its
-- conditionals do not make sense.
scanDirectives :: Text -> Maybe [Directive]
scanDirectives source = go 0 (zip [1 ..] (T.lines source))
  where
    go 0 [] = Just []
    go _ [] = Nothing -- the lines ran out inside a conditional
    go level ((n, l) : ls)
      | not (isDirective l) = go level ls
      | keyword `elem` opensGroup = at level (level + 1)
      | keyword `elem` continuesGroup, level > 0 = at (level - 1) level
      | keyword == "endif", level > 0 = at (level - 1) (level - 1)
      | keyword `notElem` conditionalKeywords = go level ls
      | otherwise = Nothing
      where
        at here next =
          (Directive {dLine = n, dKeyword = keyword, dGuard = Guard (T.stripEnd body), dLevel = here} :)
            <$> go next ls
        keyword = T.takeWhile (/= ' ') body
        body = T.stripStart (T.drop 1 (T.stripStart l))

-- | Every directive the C preprocessor takes, whether or not this module
-- can do anything with the ones it names.
directiveKeywords :: [Text]
directiveKeywords = conditionalKeywords <> opaqueKeywords

-- | The directives that ask a question, and so split a module in two.
conditionalKeywords :: [Text]
conditionalKeywords = opensGroup <> continuesGroup <> ["endif"]

-- | The keywords that open a group, continue one, and close one.
opensGroup, continuesGroup :: [Text]
opensGroup = ["if", "ifdef", "ifndef"]

continuesGroup = ["elif", "elifdef", "elifndef", "else"]

opaqueKeywords :: [Text]
opaqueKeywords =
  ["define", "undef", "include", "line", "error", "warning", "pragma"]

-- | Does this line begin with a preprocessor directive?
--
-- A hash at the start of a line is not enough to say so, which is worth
-- being careful about: the closing @#-}@ of a pragma written across several
-- lines begins one too, and that is Haskell. What settles it is the word
-- after the hash.
isDirective :: Text -> Bool
isDirective l = case T.stripPrefix "#" (T.stripStart l) of
  Just rest -> T.takeWhile isAsciiLower (T.stripStart rest) `elem` directiveKeywords
  Nothing -> False

-- | Directives that do not introduce configurations.
opaqueDirectives :: Text -> [Opaque]
opaqueDirectives source =
  [ Opaque {opLine = n, opText = T.stripEnd body, opGapBelow = blankAfter n}
    | (n, l) <- numbered,
      isDirective l,
      let body = T.stripStart (T.drop 1 (T.stripStart l)),
      T.takeWhile isAsciiLower body `elem` opaqueKeywords
  ]
  where
    numbered = zip [1 ..] (T.lines source)
    lineAt = Map.fromList numbered
    blankAfter n = maybe False (T.null . T.strip) (Map.lookup (n + 1) lineAt)

-- | One directive that asks nothing, and what is known about it.
data Opaque = Opaque
  { -- | The line it was written on.
    opLine :: Int,
    -- | What follows its hash, kept whole and never read.
    opText :: Text,
    -- | Whether a blank line was written under it.
    opGapBelow :: Bool
  }
  deriving (Eq, Show)

-- | Replace the given line ranges with empty lines, keeping every other line
-- where it was.
blanking :: [(Int, Int)] -> Text -> Text
blanking ranges source =
  T.unlines
    [ if any (holds n) ranges then "" else l
      | (n, l) <- zip [1 ..] (T.lines source)
    ]
  where
    holds n (from, to) = from <= n && n <= to

-- | Every configuration of a module, with every conditional resolved.
--
-- What 'formatWithCpp' formats, without the formatting. This is what the
-- @forall cfg@ quantifies over, and keeping it apart from the building is
-- what lets a test ask whether the building agreed with it.
leaves :: Text -> Either CppError [Text]
leaves = fmap (map snd) . answeredLeaves

-- | The configurations reached by varying one conditional at a time.
linearLeaves :: Text -> Either CppError [Text]
linearLeaves = fmap (map snd) . answeredLinearLeaves

-- | How many configurations a module has, without building any of them.
countLeaves :: Text -> Either CppError Integer
countLeaves source = case scanDirectives source of
  Nothing -> Left (UnhandledDirective (unhandledIn source))
  Just ds -> case nesting 0 ds of
    Nothing -> Left UnsplittableConditional
    Just forest ->
      Right (sum [across answers forest | answers <- combinations (afforded forest)])
  where
    across answers = product . map (one answers)
    one answers (Nest gs nested) = case lookup (gsGuards gs) answers of
      Just i -> across answers (branch nested i)
      Nothing -> sum [across answers (branch nested i) | i <- [0 .. gsCount gs - 1]]
    branch nested i = concat (take 1 (drop i nested))
    combinations = traverse (\(g, k) -> [(g, i) | i <- [0 .. k - 1]])
    afforded forest = go 1 (repeated forest)
      where
        go _ [] = []
        go n ((g, k) : rest)
          | n * toInteger k <= guardsToTie = (g, k) : go (n * toInteger k) rest
          | otherwise = []

-- | A module's conditionals as a forest: each group, with the groups nested
-- inside each of its branches.
data Nest = Nest GroupSpec [[Nest]]

-- | Read the forest off the directives, refusing a group 'groupSpec' refuses.
nesting :: Int -> [Directive] -> Maybe [Nest]
nesting level ds = traverse one (groupsAtLevel level ds)
  where
    one group = do
      gs <- groupSpec group
      Nest gs <$> traverse (\r -> nesting (level + 1) (inside r ds)) (gsBranches gs)
    inside (from, to) = filter (\d -> from <= dLine d && dLine d <= to)

-- | The guards a module asks more than once, and how many answers each has.
--
-- In the order they were written, and each named once however often it
-- appears.
repeated :: [Nest] -> [([Guard], Int)]
repeated forest = distinct Map.empty [q | q@(g, _) <- asked forest, twice g]
  where
    asked ns = concat [(gsGuards gs, gsCount gs) : asked (concat nested) | Nest gs nested <- ns]
    times = Map.fromListWith (+) [(g, 1 :: Int) | (g, _) <- asked forest]
    twice g = Map.findWithDefault 0 g times >= 2

    distinct _ [] = []
    distinct seen (q@(g, _) : rest)
      | Map.member g seen = distinct seen rest
      | otherwise = q : distinct (Map.insert g () seen) rest

-- | How many combinations of answers 'countLeaves' will enumerate.
guardsToTie :: Integer
guardsToTie = 4096

-- | Which branch every question was answered with to reach a configuration.
type Answers = Map [Guard] Int

-- | Every configuration, and the answers that reach it.
answeredLeaves :: Text -> Either CppError [(Answers, Text)]
answeredLeaves = go Map.empty
  where
    go answers source = case configurations source of
      Nothing -> (\t -> [(answers, t)]) <$> resolved source
      Just c ->
        concat
          <$> traverse
            (\(i, t) -> go (Map.insert (cfgGuards c) i answers) t)
            (zip [0 ..] (cfgTexts c))

-- | The same, labelled by the answers that reach each one, and for the same
-- reason as 'answeredLeaves'.
answeredLinearLeaves :: Text -> Either CppError [(Answers, Text)]
answeredLinearLeaves = go Map.empty
  where
    go answers source = case configurations source of
      Nothing -> (\t -> [(answers, t)]) <$> resolved source
      Just c -> case zip [0 ..] (cfgTexts c) of
        [] -> Right []
        (i, first) : rest ->
          (<>)
            <$> go (Map.insert (cfgGuards c) i answers) first
            <*> traverse (held answers (cfgGuards c)) rest
      where
        held before gs (i, t) = answeredBaseline (Map.insert gs i before) t

-- | The configuration in which every question still to be asked takes its
-- first branch, and the answers that gives.
answeredBaseline :: Answers -> Text -> Either CppError (Answers, Text)
answeredBaseline answers source = case configurations source of
  Nothing -> (answers,) <$> resolved source
  Just c -> case cfgTexts c of
    [] -> Right (answers, source)
    t : _ -> answeredBaseline (Map.insert (cfgGuards c) 0 answers) t

-- | A module with no conditionals left in it, or the reason it is not one.
resolved :: Text -> Either CppError Text
resolved source
  | any isDirective (T.lines left) =
      Left (UnhandledDirective (unhandledIn left))
  | otherwise = Right left
  where
    left = withoutOpaque source

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
