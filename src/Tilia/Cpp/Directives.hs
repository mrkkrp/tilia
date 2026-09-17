{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading the preprocessor directives of a module, and blanking lines.
module Tilia.Cpp.Directives
  ( -- * Reading a module
    usesCpp,
    blankCpp,
    withoutRuledOut,
    withoutOpaque,
    CppError (..),
    describeCppError,
    unhandledIn,

    -- * Splitting
    Guard (..),
    Configurations (..),
    configurations,
    Varied (..),
    untouched,
    leaves,
    branchLeaves,
    linearLeaves,
    countLeaves,
    answeredLeaves,
    answeredLinearLeaves,
    resolved,

    -- * The directives themselves
    Directive (..),
    scanDirectives,
    isDirective,
    GroupSpec (..),
    groupSpec,
    gsCount,
    allGroups,
    groupsAtLevel,
    blanking,
    blankingFor,
    droppedFor,
    Opaque (..),
    opaqueDirectives,
    opSpan,
    gapUnder,
    gapWritten,
  )
where

import Data.List (unsnoc)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.LanguageExtensions.Type (Extension (..))
import Tilia.Cpp.Macros (Macros, guardHolds)
import Tilia.Parser (ParseError, describeParseError)
import Tilia.Source
  ( Lines,
    blankAt,
    blankBelow,
    closesABranch,
    directiveOnLine,
  )
import Tilia.Span
  ( Span,
    mkSpan,
    spanEndLine,
    spanStartLine,
  )

----------------------------------------------------------------------------
-- Reading a module

-- | Is CPP enabled and there is at least one CPP directive present?
usesCpp :: [Extension] -> Text -> Bool
usesCpp extensions source =
  Cpp `elem` extensions && any isDirective (T.lines source)

-- | Blank out the directive lines, keeping every branch.
blankCpp :: Text -> Text
blankCpp = T.unlines . go False . T.lines
  where
    go _ [] = []
    go continuing (l : ls)
      | continuing || isDirective l = "" : go (runsOn l) ls
      | otherwise = l : go False ls
    runsOn = T.isSuffixOf "\\" . T.stripEnd

-- | Blank out every branch the macros rule out, and the conditionals that
-- ask about them.
withoutRuledOut :: Macros -> Text -> Text
withoutRuledOut macros source = case scanDirectives source of
  Nothing -> source
  Just ds ->
    blanking
      [ range
      | group <- allGroups ds,
        Just gs <- [groupSpec group],
        Just taken <- [branchTaken macros gs],
        range <- blankingFor gs taken
      ]
      source

-- | Which branch of a conditional the macros settle on.
branchTaken :: Macros -> GroupSpec -> Maybe Int
branchTaken macros = go 0 . gsGuards
  where
    go i = \case
      [] -> Just i
      g : rest -> case guardHolds macros (guardText g) of
        Just True -> Just i
        Just False -> go (i + 1) rest
        Nothing -> Nothing

-- | A module with the opaque directives blanked out.
withoutOpaque :: Text -> Text
withoutOpaque source =
  blanking [(opLine d, opLastLine d) | d <- opaqueDirectives source] source

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
  UnsplittableConditional -> "conditionals that do not nest, or an #else out of place"
  TooManyConfigurations -> "too many configurations to format"
  ConfigurationNotParsed c e -> describeParseError e <> inConfiguration c
  DirectiveUnplaceable c k -> "nowhere to put the #" <> k <> inConfiguration c
  DirectiveInQuotedText c k ->
    "a #" <> k <> " inside something quoted verbatim" <> inConfiguration c

-- | Which configuration, in words. Empty where there is only one.
inConfiguration :: [([Guard], Int)] -> Text
inConfiguration [] = ""
inConfiguration answers =
  ", in the configuration taking " <> T.intercalate ", then " (fmap said answers)
  where
    said (guards, i) = case (drop i guards, guards) of
      (g : _, _) -> "#" <> guardText g
      ([], g : _) -> "no branch of #" <> guardText g
      ([], []) -> "no branch"

-- | The keyword of the first directive here that we do not handle.
unhandledIn :: Text -> Text
unhandledIn source =
  case [ k
       | l <- T.lines source,
         Just (k, _) <- [directiveOnLine l],
         k `elem` directiveKeywords
       ] of
    k : _ -> k
    [] -> ""

----------------------------------------------------------------------------
-- Splitting

-- | One conditional directive, as written after its hash.
newtype Guard = Guard {guardText :: Text}
  deriving (Eq, Ord, Show)

-- | What one conditional splits a module into.
data Configurations = Configurations
  { -- | The directives: the @#if@ of the group, and then one per @#elif@.
    cfgGuards :: [Guard],
    -- | One module text per branch, in the same order as the directives,
    -- and then one more for the @#else@.
    cfgTexts :: [Text],
    -- | What each of those branches leaves out, in the same order.
    cfgDropped :: [[(Int, Int)]],
    -- | From each tied group's @#if@ to its @#endif@, inclusive.
    cfgWholes :: Varied
  }
  deriving (Eq, Show)

-- | Split a module on its first outermost conditional, and on every other
-- one written behind the same directives, wherever in the module it sits.
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
        cfgDropped =
          [concatMap (`droppedFor` i) tied | i <- [0 .. gsCount gs - 1]],
        cfgWholes = Varied (fmap gsWhole tied)
      }

-- | Every group in a module written behind the same directives as this one.
sameGuard :: GroupSpec -> [Directive] -> [GroupSpec]
sameGuard gs ds =
  [ g
  | grp <- allGroups ds,
    Just g <- [groupSpec grp],
    gsGuards g == gsGuards gs
  ]

-- | The lines one conditional could have printed differently.
newtype Varied = Varied {variedLines :: [(Int, Int)]}
  deriving (Eq, Show)

-- | Was this region printed from lines the conditional left alone?
untouched :: Varied -> Span -> Bool
untouched (Varied ranges) s = not (any reaches ranges)
  where
    reaches (from, to) = spanStartLine s <= to && from <= spanEndLine s

-- | Every configuration of a module, with every conditional resolved.
leaves :: Text -> Either CppError [Text]
leaves = fmap (fmap snd) . answeredLeaves

-- | One configuration for every branch of every conditional, and no more.
branchLeaves :: Text -> Either CppError [Text]
branchLeaves source = case scanDirectives source of
  Nothing -> Left (UnhandledDirective (unhandledIn source))
  Just ds -> case nesting 0 ds of
    Nothing -> Left UnsplittableConditional
    Just forest -> traverse resolved (distinct (fmap configuration (assignments forest)))
  where
    reachable = go Map.empty
      where
        go asked ns =
          concat
            [ (asked, gs)
                : concat
                  [ go (Map.insert (gsGuards gs) i asked) nested
                  | (i, nested) <- zip [0 ..] branches
                  ]
            | Nest gs branches <- ns
            ]
    assignments forest =
      Map.empty
        : [ Map.insert (gsGuards gs) i asked
          | (asked, gs) <- reachable forest,
            i <- [0 .. gsCount gs - 1]
          ]
    configuration answers =
      blanking
        [ r
        | grp <- allGroups (concat (maybeToList (scanDirectives source))),
          Just gs <- [groupSpec grp],
          r <- blankingFor gs (Map.findWithDefault 0 (gsGuards gs) answers)
        ]
        source
    distinct = Map.elems . Map.fromList . fmap (\t -> (t, t))

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

-- | The configurations reached by varying one conditional at a time.
linearLeaves :: Text -> Either CppError [Text]
linearLeaves = fmap (fmap snd) . answeredLinearLeaves

-- | How many configurations a module has, without building any of them.
countLeaves :: Text -> Either CppError Integer
countLeaves source = case scanDirectives source of
  Nothing -> Left (UnhandledDirective (unhandledIn source))
  Just ds -> case nesting 0 ds of
    Nothing -> Left UnsplittableConditional
    Just forest ->
      Right (sum [across answers forest | answers <- combinations (afforded forest)])
  where
    across answers = product . fmap (one answers)
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

-- | The guards a module asks more than once, and how many answers each has.
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

-- | Which branch every question was answered with to reach a configuration.
type Answers = Map [Guard] Int

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
-- The directives themselves

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
      | Nothing <- directiveOnLine l = go level ls
      | keyword `elem` opensGroup = at level (level + 1)
      | keyword `elem` continuesGroup, level > 0 = at (level - 1) level
      | keyword == "endif", level > 0 = at (level - 1) (level - 1)
      | keyword `notElem` conditionalKeywords = go level ls
      | otherwise = Nothing
      where
        at here next =
          ( Directive
              { dLine = n,
                dKeyword = keyword,
                dGuard = Guard (T.stripEnd body),
                dLevel = here
              }
              :
          )
            <$> go next ls
        (keyword, body) = fromMaybe ("", "") (directiveOnLine l)

-- | The directives that ask a question, and so split a module in two.
conditionalKeywords :: [Text]
conditionalKeywords = opensGroup <> continuesGroup <> ["endif"]

-- | The keywords that open a group or continue one.
opensGroup, continuesGroup :: [Text]
opensGroup = ["if", "ifdef", "ifndef"]
continuesGroup = ["elif", "elifdef", "elifndef", "else"]

-- | Does this line begin with a preprocessor directive?
isDirective :: Text -> Bool
isDirective l = case directiveOnLine l of
  Just (keyword, _) -> keyword `elem` directiveKeywords
  Nothing -> False

-- | Every directive the C preprocessor takes, whether or not this module
-- can do anything with the ones it names.
directiveKeywords :: [Text]
directiveKeywords = conditionalKeywords <> opaqueKeywords

-- | The unconditional directives that do not split the source code.
opaqueKeywords :: [Text]
opaqueKeywords =
  ["define", "undef", "include", "line", "error", "warning", "pragma"]

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
        gsOwnLines = fmap dLine group,
        gsBranches = [(dLine a + 1, dLine b - 1) | (a, b) <- zip group (drop 1 group)],
        gsWhole = (dLine opener, dLine end)
      }
  where
    require b = if b then Just () else Nothing

-- | How many configurations a group has: one per condition, and one more
-- for when none of them holds.
gsCount :: GroupSpec -> Int
gsCount gs = length (gsGuards gs) + 1

-- | Every conditional in a module, at whatever depth it sits.
allGroups :: [Directive] -> [[Directive]]
allGroups ds = concat [groupsAtLevel l ds | l <- [0 .. deepest]]
  where
    deepest = maximum (0 : fmap dLevel ds)

-- | The directives at one level of nesting, split into the groups they make
-- up.
groupsAtLevel :: Int -> [Directive] -> [[Directive]]
groupsAtLevel level = split . filter ((== level) . dLevel)
  where
    split ds = case break ((== "endif") . dKeyword) ds of
      (_, []) -> []
      (before', end : rest) -> (before' <> [end]) : split rest

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

-- | The lines to blank so that configuration @i@ of a group is what is left.
--
-- The branches this configuration does not take, and the directives
-- themselves: a directive belongs to no configuration, which is the whole of
-- what separates this from 'droppedFor'.
blankingFor :: GroupSpec -> Int -> [(Int, Int)]
blankingFor gs i = droppedFor gs i <> [(n, n) | n <- gsOwnLines gs]

-- | The lines a configuration of a group is not including.
droppedFor :: GroupSpec -> Int -> [(Int, Int)]
droppedFor gs i
  | i < length (gsGuards gs) || gsHasElse gs =
      [r | (k, r) <- zip [0 :: Int ..] (gsBranches gs), k /= i]
  | otherwise = gsBranches gs

-- | One opaque directive and what is known about it.
data Opaque = Opaque
  { -- | The line it was written on.
    opLine :: Int,
    -- | The last line it takes up, which is 'opLine' unless it was written
    -- across several with backslashes.
    opLastLine :: Int,
    -- | What follows its hash, kept whole and never read.
    opText :: Text
  }
  deriving (Eq, Show)

-- | Directives that do not introduce configurations.
opaqueDirectives :: Text -> [Opaque]
opaqueDirectives source =
  [ Opaque
      { opLine = n,
        opLastLine = end n,
        opText = T.stripEnd (T.intercalate "\n" (body : fmap lineOf below))
      }
  | (n, l) <- numbered,
    Just (keyword, body) <- [directiveOnLine l],
    keyword `elem` opaqueKeywords,
    let below = continuing n
  ]
  where
    numbered = zip [1 ..] (T.lines source)
    byLine = Map.fromList numbered
    lineOf n = Map.findWithDefault "" n byLine
    end n = last (n : continuing n)
    continuing n
      | maybe False runsOn (Map.lookup n byLine) = n + 1 : continuing (n + 1)
      | otherwise = []
    runsOn = T.isSuffixOf "\\" . T.stripEnd

-- | The lines a directive was written on, as a span.
opSpan :: Opaque -> Span
opSpan d = mkSpan (opLine d, 1) (opLastLine d, 1)

-- | Did the author leave an empty line under this directive?
gapUnder :: Lines -> Opaque -> Bool
gapUnder written d =
  (blankAt n written || blankBelow n written) && not (closesABranch n written)
  where
    n = opLastLine d

-- | Did the author leave an empty line anywhere between these two lines?
gapWritten :: Lines -> Int -> Int -> Bool
gapWritten written from to = any (`blankAt` written) [from .. to]
