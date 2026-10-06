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
    everyBranch,
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

import Control.Applicative ((<|>))
import Control.Monad (join, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, except, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Foldable (traverse_)
import Data.IntMap.Strict qualified as IntMap
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Monoid (Any (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Tilia.Cpp.Directives
import Tilia.Cpp.Fragment (bodyOf, fragmentText, fragmentsOf, linesHeld, reassembled)
import Tilia.Cpp.Merge (Settled (..), combine, merge)
import Tilia.Cpp.Place (CommentSummary, restoreUnprinted, summarizeComments)
import Tilia.Doc (defaultRenderOptions, printDoc)
import Tilia.Doc.Internal
  ( Doc (..),
    Layout (..),
    conditionalRange,
    foldChildren,
    mapChildren,
    printedFrom,
  )
import Tilia.Parser
  ( ParsedModule,
    ParserConfig,
    importLayout,
    parseConfiguration,
    parseModule,
    pmSource,
    readAlike,
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
    evalState
      ( runExceptT
          ( formatAllConfigs
              parser
              (knowing render)
              path
              (noAnswers source)
              source
          )
      )
      (configurationBudget * linesHeld source)
  formatted <-
    printDoc defaultRenderOptions
      <$> restoreUnprinted source found document
  formatted <$ traverse_ (Left . RuledOutBranch) (ruledOutBranch formatted)
  where
    knowing c =
      c
        { rcImportBarriers =
            maybe
              []
              (importBarriers parser source . allGroups)
              (scanConditionals source)
        }

-- | A module's branches, parsed: as one module where they parse together,
-- and otherwise as the branch leaves that parse.
everyBranch ::
  -- | What to parse with.
  ParserConfig ->
  -- | The file this is, for the positions in a parse error.
  FilePath ->
  -- | The module, directives and all.
  Text ->
  -- | 'Nothing' where nothing parses.
  Maybe (NonEmpty ParsedModule)
everyBranch parser path source =
  case parseModule parser path (blankCpp source) of
    Right whole -> Just (pure whole)
    Left _ ->
      nonEmpty
        [ parsed
        | Right texts <- [branchLeaves source],
          Right parsed <- fmap (parseModule parser path) texts
        ]

-- | The lines imports must not be sorted across: the directives of every
-- conditional but one that only continues the item above it, such as a
-- @hiding@ clause written behind a condition.
importBarriers ::
  -- | What to read the module with.
  ParserConfig ->
  -- | The module, directives and all.
  Text ->
  -- | Every conditional of the module, nested ones included.
  [GroupSpec] ->
  [Int]
importBarriers parser source groups =
  sort [l | g <- groups, not (continuesAnItem g), l <- gsOwnLines g]
  where
    starts = fromMaybe IntMap.empty (importLayout parser (blankCpp source))
    continuesAnItem g =
      let (from, to) = gsWhole g
          held =
            IntMap.elems $
              fst (IntMap.split to (snd (IntMap.split from starts)))
       in not (null held) && not (or held)

-- | Formatting that spends the lines it formats out of the budget left, or
-- fails with a 'CppError'.
type Spending = ExceptT CppError (State Int)

-- | Give up on formatting the module.
refuse :: CppError -> Spending a
refuse = throwE

-- | Take lines out of the budget, or give up where fewer are left.
spend :: Int -> Spending ()
spend n = do
  budget <- lift get
  when (budget < n) (refuse TooManyConfigurations)
  lift (put (budget - n))

-- | What formatting comes to, or 'Nothing' where it fails for a reason
-- other than the budget running out. What it spent stays spent.
attempt :: Spending a -> Spending (Maybe a)
attempt formatting =
  (Just <$> formatting) `catchE` \case
    TooManyConfigurations -> refuse TooManyConfigurations
    _ -> pure Nothing

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
  forest <- except (readConditionals source)
  case variations forest source of
    Nothing -> do
      spend (linesHeld source)
      except (formatSingleConfig parser render path reached (withoutOpaque source))
    Just apart -> do
      budget <- lift get
      let pragma = splitOnPragma parser forest source
          least = max 1 (linesHeld (blanking (fmap gsWhole forest) source))
          affordable = linearCost (budget `div` least) forest source * least <= budget
      base <-
        if isNothing pragma || affordable
          then
            attempt $
              formatAllConfigs
                parser
                render
                path
                (without (vaBaselineDropped apart) reached)
                (vaBaseline apart)
          else pure Nothing
      fragmented <- case (pragma, base) of
        (Nothing, Just b) -> join <$> attempt (inFragments parser render path reached forest source b)
        _ -> pure Nothing
      linear <- case (fragmented, base) of
        (Nothing, Just b) | affordable -> oneAtATime apart b
        _ -> pure Nothing
      case fragmented <|> linear of
        Just found -> pure found
        Nothing -> case maybeToList pragma <> forest of
          g : _ -> together parser render path reached (configurationsOn g forest source)
          [] -> error "Tilia: a module that varies has a conditional to split on"
  where
    oneAtATime apart base = do
      varied <- attempt (separately parser render path reached apart base)
      pure $ do
        (merged, found) <- varied
        d <- combine Broken (fst base) (zip (fmap cfgWholes (vaGroups apart)) merged)
        pure (d, found)

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
                    cfgConditionals = [gsConditional gs]
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
  -- | What the baseline was formatted to.
  (Doc, CommentSummary) ->
  Spending ([Doc], CommentSummary)
separately parser render path reached v (baseDoc, baseFound) = do
  groups <- for (vaGroups v) $ \c ->
    for (zip [0 ..] (cfgTexts c)) $ \(i, t) ->
      if t == vaBaseline v
        then pure (baseDoc, mempty)
        else formatAllConfigs parser render path (answering c i reached) t
  pure
    ( zipWith
        (mergeOf (reachedLines reached))
        (vaGroups v)
        (fmap (fmap fst) groups),
      baseFound <> foldMap (foldMap snd) groups
    )

-- | Format each fragment of declarations the outermost conditionals reach
-- on its own, and put the results into what the baseline was formatted to.
--
-- 'Nothing' where the module cannot be taken apart like that.
inFragments ::
  -- | What to parse a configuration with.
  ParserConfig ->
  -- | What to print it with.
  RenderConfig ->
  -- | The file this is, for the positions in a parse error.
  FilePath ->
  -- | How this configuration was reached.
  Reached ->
  -- | The outermost conditionals.
  [GroupSpec] ->
  -- | Input text.
  Text ->
  -- | What the baseline was formatted to.
  (Doc, CommentSummary) ->
  Spending (Maybe (Doc, CommentSummary))
inFragments parser render path reached forest source (baseDoc, baseFound) =
  case parts of
    Nothing -> pure Nothing
    Just (body, ps) -> do
      formatted <- for ps $ \(f, (text, dropped)) ->
        (,) f
          <$> formatAllConfigs
            parser
            render
            path
            reached{reachedLines = dropping dropped (reachedLines reached)}
            text
      pure $ do
        doc <- reassembled body [(f, d) | (f, (d, _)) <- formatted]
        pure (doc, baseFound <> foldMap (snd . snd) formatted)
  where
    parts = do
      body <- bodyOf baseDoc
      ps <-
        traverse
          (\f -> (,) f <$> fragmentText body forest source f)
          (fragmentsOf body forest)
      pure (body, ps)

-- | The outermost conditional around a @LANGUAGE@ or @OPTIONS@ pragma that
-- changes how the rest of the module is parsed, if there is one.
--
-- Formatting a fragment on its own takes the rest of the module to be
-- parsed alike in every configuration.
splitOnPragma :: ParserConfig -> [GroupSpec] -> Text -> Maybe GroupSpec
splitOnPragma parser forest source =
  listToMaybe
    [ o
    | g <- groups,
      holdsPragma g,
      any (not . readAlike parser baseline . answered g) [1 .. gsCount g - 1],
      o <- forest,
      fst (gsWhole o) <= fst (gsWhole g),
      snd (gsWhole g) <= snd (gsWhole o)
    ]
  where
    groups = allGroups forest
    baseline = blanking (concatMap (`blankingFor` 0) groups) source
    answered g i =
      blanking
        (blankingFor g i <> concat [blankingFor h 0 | h <- groups, gsWhole h /= gsWhole g])
        source
    written = T.lines source
    holdsPragma g =
      let (from, to) = gsWhole g
       in any pragma (take (to - from + 1) (drop (from - 1) written))
    pragma l =
      let u = T.toUpper l
       in "{-#" `T.isInfixOf` u
            && ("LANGUAGE" `T.isInfixOf` u || "OPTIONS" `T.isInfixOf` u)

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
  (found, blind) <-
    formatted differentlyRead `catchE` \case
      ConfigurationNotParsed answers _
        | not differentlyRead,
          isNothing (implied answers) ->
            formatted True
      e -> refuse e
  if blind then fst <$> formatted False else pure found
  where
    differentlyRead =
      or (zipWith (\a b -> not (readAlike parser a b)) texts (drop 1 texts))
    texts = cfgTexts c
    formatted settle = do
      let (settledBy, answers) =
            unzip . (if settle then settling else fmap ((,) [])) $
              [(answering c i reached, t) | (i, t) <- zip [0 ..] texts]
      found <- fmap catMaybes . for (zip [0 ..] answers) $ \(i, (r, t)) ->
        if null (unconditionalErrors t)
          then Just . (,) i <$> formatAllConfigs parser render path r t
          else pure Nothing
      docs <-
        except (traverse (complete (fmap (fmap fst) found)) (zip [0 ..] texts))
      let settledIn i
            | Just _ <- lookup i found = settledBy !! i
            | otherwise = foldMap ((settledBy !!) . fst) (listToMaybe found)
          settled = settledAcross (fmap settledIn [0 .. length answers - 1])
          ranges = mapMaybe (conditionalRange . settledConditional) settled
          varied = Varied (variedLines (cfgWholes c) <> ranges)
          merged =
            merge
              (reachedLines reached)
              (cfgConditionals c)
              (cfgGuards c)
              varied
              settled
              docs
      pure ((merged, foldMap (snd . snd) found), blindOver ranges merged)
    blindOver ranges d = case d of
      DCppChoice [] bs _
        | fmap fst bs == fmap guardText (cfgGuards c),
          any (\(a, b) -> any (\(from, to) -> a <= to && from <= b) ranges) $
            printedFrom d ->
            True
      _ -> getAny (foldChildren (Any . blindOver ranges) d)
    complete found (i, t) = case lookup i found of
      Just d -> Right d
      Nothing -> case listToMaybe found >>= errorBranch (cfgWholes c) t . snd of
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

-- | Take, in every conditional the answers to one question settle, the
-- branch they settle it on, as the preprocessor would, where an answer
-- leaving it open prints its other branches.
settling ::
  -- | Each answer, and the configuration it gives.
  [(Reached, Text)] ->
  -- | Each answer's settled conditionals and its configuration with them
  -- taken.
  [([(GroupSpec, Int)], (Reached, Text))]
settling given =
  [ (settled, (reached', blanking (concatMap (uncurry blankingFor) settled) t))
  | ((reached, t), (candidates, _)) <- zip given found,
    let settled = [s | s@(gs, _) <- candidates, Set.member (gsWhole gs) open],
    let reached' =
          reached
            { reachedAnswers =
                reachedAnswers reached
                  <> [(gsGuards gs, k) | (gs, k) <- settled],
              reachedLines =
                dropping
                  (concatMap (uncurry droppedFor) settled)
                  (reachedLines reached)
            }
  ]
  where
    found =
      [ if null (unconditionalErrors t)
          then walk (implied (reachedAnswers r)) (scanConditionals t)
          else ([], [])
      | (r, t) <- given
      ]
    open = Set.fromList (concatMap snd found)
    walk (Just known) (Just forest) = foldMap (visit known) forest
    walk _ _ = ([], [])
    visit known gs = case settledBranch known gs of
      Just k -> ([(gs, k)], []) <> foldMap (visit known) (nestedIn gs k)
      Nothing ->
        ([], [gsWhole gs]) <> foldMap (foldMap (visit known)) (gsNested gs)

-- | The conditionals some answer settles, given what each answer settles.
settledAcross :: [[(GroupSpec, Int)]] -> [Settled]
settledAcross perAnswer =
  [ Settled
      { settledConditional = gsConditional gs,
        settledBranches =
          [ lookup (gsWhole gs) [(gsWhole g, k) | (g, k) <- ss]
          | ss <- perAnswer
          ]
      }
  | gs <- Map.elems (Map.fromList [(gsWhole g, g) | (g, _) <- concat perAnswer])
  ]

-- | How many times over one call may format the lines of a module.
configurationBudget :: Int
configurationBudget = 64

-- | Merge the documents one group's configurations printed to.
mergeOf :: Lines -> Configurations -> [Doc] -> Doc
mergeOf written c =
  merge
    written
    (cfgConditionals c)
    (cfgGuards c)
    (cfgWholes c)
    []

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
