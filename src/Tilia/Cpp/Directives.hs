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
    Malformation (..),
    describeCppError,

    -- * Splitting
    Guard (..),
    Configurations (..),
    configurations,
    configurationsOn,
    Varied (..),
    untouched,
    leaves,
    branchLeaves,
    correspondingBranches,
    ruledOutBranch,
    unconditionalErrors,
    linearLeaves,
    countLeaves,
    answeredLeaves,
    answeredLinearLeaves,
    resolved,

    -- * The directives themselves
    Directive (..),
    dSpan,
    readConditionals,
    scanConditionals,
    isDirective,
    GroupSpec (..),
    gsOwnLines,
    gsCount,
    nestedIn,
    allGroups,
    blanking,
    blankingFor,
    droppedFor,
    opaqueDirectives,
  )
where

import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.LanguageExtensions.Type (Extension (..))
import Tilia.Cpp.Macros (Macros, guardHolds)
import Tilia.Doc.Internal (Conditional (..))
import Tilia.Parser (ParseError, describeParseError)
import Tilia.Source (directiveOnLine)
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
blankCpp source = blanking [(dLine d, dLastLine d) | d <- directives source] source

-- | Blank out every branch the macros rule out, and the conditionals that
-- ask about them.
withoutRuledOut :: Macros -> Text -> Text
withoutRuledOut macros source = case scanConditionals source of
  Nothing -> source
  Just forest ->
    blanking
      [ range
      | gs <- allGroups forest,
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
  blanking [(dLine d, dLastLine d) | d <- opaqueDirectives source] source

-- | Why a module using the preprocessor could not be formatted.
data CppError
  = -- | A conditional that does not make sense: the line and the keyword of
    -- the directive that gives it away, and what is wrong with it.
    MalformedConditional Int Text Malformation
  | -- | More configurations than 'configurationBudget' allows.
    TooManyConfigurations
  | -- | A configuration the parser rejected, and which one it was.
    ConfigurationNotParsed [([Guard], Int)] ParseError
  | -- | A directive written inside a quasi-quote or a multi-line string, its
    -- line, and its keyword.
    DirectiveInQuotedText Int Text
  | -- | A branch holding something that a conditional around it, asking the
    -- same question, rules out, and the line of the directive opening it.
    RuledOutBranch Int
  | -- | An alternative that aborts with @#error@ that cannot be formatted
    -- without parsing it, and the line of the @#error@.
    AbortingAlternative Int

-- | What is wrong with a conditional directive.
data Malformation
  = -- | It opens a conditional that nothing closes.
    NeverClosed
  | -- | It continues or closes a conditional where none is open.
    NothingOpen
  | -- | It continues a conditional after its @#else@.
    AfterElse

-- | Say what went wrong, in one line. The edge of the system.
describeCppError :: CppError -> Text
describeCppError = \case
  MalformedConditional n k m ->
    "the #" <> k <> " at line " <> T.pack (show n) <> case m of
      NeverClosed -> " is never closed"
      NothingOpen -> " has no conditional to belong to"
      AfterElse -> " comes after the #else of its conditional"
  TooManyConfigurations -> "too many configurations to format"
  ConfigurationNotParsed c e -> describeParseError e <> inConfiguration c
  DirectiveInQuotedText n k ->
    "the #" <> k <> " at line " <> T.pack (show n) <> " is inside a quasi-quote or a multi-line string"
  RuledOutBranch n ->
    "the branch at line "
      <> T.pack (show n)
      <> " is ruled out by a conditional around it that asks the same question"
  AbortingAlternative n ->
    "the alternative that the #error at line "
      <> T.pack (show n)
      <> " aborts cannot be formatted without parsing it"

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

----------------------------------------------------------------------------
-- Splitting

-- | One conditional directive, as written after its hash.
newtype Guard = Guard{guardText :: Text}
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
    cfgWholes :: Varied,
    -- | Each tied group as its author wrote it, in the same order.
    cfgConditionals :: [Conditional]
  }
  deriving (Eq, Show)

-- | Split a module on its first outermost conditional, and on every other
-- one written behind the same directives, wherever in the module it sits.
configurations :: Text -> Maybe Configurations
configurations source = do
  forest <- scanConditionals source
  gs <- listToMaybe forest
  pure (configurationsOn gs forest source)

-- | Split a module on one of its conditionals, and on every other one
-- written behind the same directives.
configurationsOn ::
  -- | The conditional.
  GroupSpec ->
  -- | The module's conditionals.
  [GroupSpec] ->
  -- | The module.
  Text ->
  Configurations
configurationsOn gs forest source =
  Configurations
    { cfgGuards = gsGuards gs,
      cfgTexts =
        [ blanking (concatMap (`blankingFor` i) tied) source
        | i <- [0 .. gsCount gs - 1]
        ],
      cfgDropped =
        [concatMap (`droppedFor` i) tied | i <- [0 .. gsCount gs - 1]],
      cfgWholes = Varied (fmap gsWhole tied),
      cfgConditionals = fmap gsConditional tied
    }
  where
    tied = sameGuard gs forest

-- | Every group in a module written behind the same directives as this one.
sameGuard :: GroupSpec -> [GroupSpec] -> [GroupSpec]
sameGuard gs forest = [g | g <- allGroups forest, gsGuards g == gsGuards gs]

-- | The lines one conditional could have printed differently.
newtype Varied = Varied{variedLines :: [(Int, Int)]}
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
branchLeaves source = do
  forest <- readConditionals source
  traverse
    resolved
    ( filter
        (null . unconditionalErrors)
        (distinct (fmap (\a -> configurationOf forest a source) (branchAssignments forest)))
    )
  where
    distinct = Map.elems . Map.fromList . fmap (\t -> (t, t))

-- | The empty assignment, and then one for every branch of every
-- conditional, which reaches the conditional and takes the branch.
branchAssignments :: [GroupSpec] -> [Assignment]
branchAssignments forest =
  Map.empty
    : [ Map.insert (gsGuards gs) i asked
      | (asked, gs) <- reachable Map.empty forest,
        i <- [0 .. gsCount gs - 1]
      ]
  where
    reachable asked gss =
      concat
        [ (asked, gs)
            : concat
              [ reachable (Map.insert (gsGuards gs) i asked) nested
              | (i, nested) <- zip [0 ..] (gsNested gs)
              ]
        | gs <- gss
        ]

-- | The configuration of a module an assignment selects, a conditional it
-- does not mention taking its first branch.
configurationOf :: [GroupSpec] -> Assignment -> Text -> Text
configurationOf forest assignment =
  blanking
    [ r
    | gs <- allGroups forest,
      r <- blankingFor gs (Map.findWithDefault 0 (gsGuards gs) assignment)
    ]

-- | The line of the first directive whose branch holds something although
-- a conditional around it, asking the same question, rules that branch out.
ruledOutBranch :: Text -> Maybe Int
ruledOutBranch source = do
  forest <- scanConditionals source
  listToMaybe (go Map.empty forest)
  where
    written = zip [1 ..] (T.lines source)
    holdsSomething (from, to) =
      any (\(n, l) -> from <= n && n <= to && not (T.all isSpace l)) written
    go asked forest =
      concat
        [ [ opening
          | Just j <- [Map.lookup (gsGuards gs) asked],
            (i, opening, r) <- zip3 [0 :: Int ..] (gsOwnLines gs) (gsBranches gs),
            i /= j,
            holdsSomething r
          ]
            <> concat
              [ go (Map.insert (gsGuards gs) i asked) nested
              | (i, nested) <- zip [0 ..] (gsNested gs)
              ]
        | gs <- forest
        ]

-- | Read both spellings under the same CPP choices.
--
-- Sorting or deduplicating the resulting source text separately loses the
-- association with the guards: formatting can change that order or make two
-- formerly different strings identical. Cover every branch of either
-- spelling, including its ancestors. 'Nothing' denotes a configuration
-- deliberately rejected by @#error@.
correspondingBranches ::
  -- | Before.
  Text ->
  -- | After.
  Text ->
  Either CppError [(Maybe Text, Maybe Text)]
correspondingBranches before after = do
  left <- readConditionals before
  right <- readConditionals after
  let defaults = Map.fromList [(gsGuards gs, 0) | gs <- allGroups (left <> right)]
      choices = Map.keys (Map.fromList [(Map.union a defaults, ()) | a <- branchAssignments left <> branchAssignments right])
  traverse (\a -> (,) <$> reading before left a <*> reading after right a) choices
  where
    reading source forest assignment =
      let selected = configurationOf forest assignment source
       in if null (unconditionalErrors selected)
            then Just <$> resolved selected
            else Right Nothing

-- | An unconditional @#error@ means this configuration has no Haskell
-- program to parse. Conditional errors are only considered after choosing a
-- branch.
unconditionalErrors :: Text -> [Directive]
unconditionalErrors source =
  [ d
  | d <- opaqueDirectives source,
    dKeyword d == "error",
    not (any (encloses (dLine d)) groups)
  ]
  where
    groups = [gsWhole g | forest <- maybeToList (scanConditionals source), g <- allGroups forest]
    encloses n (from, to) = from < n && n < to

-- | The configurations reached by varying one conditional at a time.
linearLeaves :: Text -> Either CppError [Text]
linearLeaves = fmap (fmap snd) . answeredLinearLeaves

-- | How many configurations a module has, without building any of them.
countLeaves :: Text -> Either CppError Integer
countLeaves source = do
  forest <- readConditionals source
  pure (sum [across answers forest | answers <- combinations (afforded forest)])
  where
    across answers = product . fmap (one answers)
    one answers gs = case lookup (gsGuards gs) answers of
      Just i -> across answers (nestedIn gs i)
      Nothing -> sum [across answers (nestedIn gs i) | i <- [0 .. gsCount gs - 1]]
    combinations = traverse (\(g, k) -> [(g, i) | i <- [0 .. k - 1]])
    afforded forest = go 1 (repeated forest)
      where
        go _ [] = []
        go n ((g, k) : rest)
          | n * toInteger k <= guardsToTie = (g, k) : go (n * toInteger k) rest
          | otherwise = []

-- | The guards a module asks more than once, and how many answers each has.
repeated :: [GroupSpec] -> [([Guard], Int)]
repeated forest = distinct Map.empty [q | q@(g, _) <- asked forest, twice g]
  where
    asked gss = concat [(gsGuards gs, gsCount gs) : asked (concat (gsNested gs)) | gs <- gss]
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
answeredLeaves :: Text -> Either CppError [(Assignment, Text)]
answeredLeaves = go Map.empty
  where
    go answers source = case configurations source of
      Nothing | not (null (unconditionalErrors source)) -> Right []
      Nothing -> (\t -> [(answers, t)]) <$> resolved source
      Just c ->
        concat
          <$> traverse
            (\(i, t) -> go (Map.insert (cfgGuards c) i answers) t)
            (zip [0 ..] (cfgTexts c))

-- | Which branch every question was answered with to reach a configuration.
type Assignment = Map [Guard] Int

-- | The same, labelled by the answers that reach each one, and for the same
-- reason as 'answeredLeaves'.
answeredLinearLeaves :: Text -> Either CppError [(Assignment, Text)]
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
answeredBaseline :: Assignment -> Text -> Either CppError (Assignment, Text)
answeredBaseline answers source = case configurations source of
  Nothing -> (answers,) <$> resolved source
  Just c -> case cfgTexts c of
    [] -> Right (answers, source)
    t : _ -> answeredBaseline (Map.insert (cfgGuards c) 0 answers) t

-- | A module with every conditional answered, and its other directives
-- taken out, or why its conditionals do not make sense.
resolved :: Text -> Either CppError Text
resolved source = withoutOpaque source <$ readConditionals source

----------------------------------------------------------------------------
-- The directives themselves

-- | One preprocessor directive, as written.
data Directive = Directive
  { dLine :: !Int,
    -- | The last line the directive is written on, which is its first unless
    -- a line of it ends in a backslash.
    dLastLine :: !Int,
    dKeyword :: !Text,
    -- | What follows its hash, on every line it is written on.
    dText :: !Text
  }
  deriving (Eq, Show)

-- | The lines a directive was written on, as a span.
dSpan :: Directive -> Span
dSpan d = mkSpan (dLine d, 1) (dLastLine d, 1)

-- | Every directive in a module, in the order they are written.
directives :: Text -> [Directive]
directives = go . zip [1 ..] . T.lines
  where
    go = \case
      [] -> []
      (n, l) : ls
        | Just (keyword, body) <- directiveOnLine l,
          keyword `elem` directiveKeywords ->
            let (continued, rest) = continuation l ls
             in Directive
                  { dLine = n,
                    dLastLine = n + length continued,
                    dKeyword = keyword,
                    dText = T.intercalate "\n" (fmap T.stripEnd (body : continued))
                  }
                  : go rest
        | otherwise -> go ls
    continuation l ls
      | T.isSuffixOf "\\" (T.stripEnd l),
        (_, next) : rest <- ls =
          let (more, rest') = continuation next rest in (next : more, rest')
      | otherwise = ([], ls)

-- | A module's conditionals, each with the ones inside its branches, or why
-- they do not make sense.
readConditionals :: Text -> Either CppError [GroupSpec]
readConditionals = go [] [] . filter ((`elem` conditionalKeywords) . dKeyword) . directives
  where
    -- The groups finished where the reading is, last first, and the
    -- conditionals open around it, innermost first.
    go done open = \case
      [] -> case open of
        [] -> Right (reverse done)
        o : _ -> Left (MalformedConditional (dLine (oOpener o)) (dKeyword (oOpener o)) NeverClosed)
      d : ds
        | dKeyword d `elem` opensGroup -> go [] (Open d [] [] done : open) ds
        | dKeyword d `elem` continuesGroup -> case open of
            [] -> malformed NothingOpen
            o : rest
              | any ((== "else") . dKeyword) (take 1 (oLater o)) -> malformed AfterElse
              | otherwise ->
                  go [] (o{oLater = d : oLater o, oEarlier = reverse done : oEarlier o} : rest) ds
        | otherwise -> case open of
            [] -> malformed NothingOpen
            o : rest ->
              let gs =
                    groupSpec
                      (oOpener o)
                      (reverse (oLater o))
                      d
                      (reverse (reverse done : oEarlier o))
               in go (gs : oBefore o) rest ds
        where
          malformed = Left . MalformedConditional (dLine d) (dKeyword d)

-- | A conditional still open as the directives are read.
data Open = Open
  { -- | The directive that opened it.
    oOpener :: Directive,
    -- | The directives that continued it, last first.
    oLater :: [Directive],
    -- | The groups inside each of its branches read so far, last first.
    oEarlier :: [[GroupSpec]],
    -- | The groups before it where it is, last first.
    oBefore :: [GroupSpec]
  }

-- | A module's conditionals, or 'Nothing' if they do not make sense.
scanConditionals :: Text -> Maybe [GroupSpec]
scanConditionals = either (const Nothing) Just . readConditionals

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
    gsConditional :: Conditional,
    gsOwnRanges :: [(Int, Int)],
    gsBranches :: [(Int, Int)],
    gsWhole :: (Int, Int),
    -- | The groups inside each of its branches.
    gsNested :: [[GroupSpec]]
  }

-- | Read a group off its directives.
groupSpec ::
  -- | The directive opening it.
  Directive ->
  -- | The ones continuing it.
  [Directive] ->
  -- | The one closing it.
  Directive ->
  -- | The groups inside each of its branches.
  [[GroupSpec]] ->
  GroupSpec
groupSpec opener later end nested =
  GroupSpec
    { gsGuards = [Guard (dText d) | d <- opener : later, dKeyword d /= "else"],
      gsHasElse = any ((== "else") . dKeyword) later,
      gsConditional =
        Conditional
          { conditionalLines = fmap dLine group,
            conditionalElse = foldMap afterKeyword (filter ((== "else") . dKeyword) later),
            conditionalEndif = afterKeyword end
          },
      gsOwnRanges = [(dLine d, dLastLine d) | d <- group],
      gsBranches = [(dLastLine a + 1, dLine b - 1) | (a, b) <- zip group (drop 1 group)],
      gsWhole = (dLine opener, dLastLine end),
      gsNested = nested
    }
  where
    group = opener : later <> [end]
    afterKeyword d = T.drop (T.length (dKeyword d)) (dText d)

-- | The lines of a group's directives, the @#if@ first and the @#endif@ last.
gsOwnLines :: GroupSpec -> [Int]
gsOwnLines = conditionalLines . gsConditional

-- | How many configurations a group has: one per condition, and one more
-- for when none of them holds.
gsCount :: GroupSpec -> Int
gsCount gs = length (gsGuards gs) + 1

-- | The groups inside the branch configuration @i@ of a group takes.
nestedIn :: GroupSpec -> Int -> [GroupSpec]
nestedIn gs i = concat (take 1 (drop i (gsNested gs)))

-- | Every conditional in a module, outermost first.
allGroups :: [GroupSpec] -> [GroupSpec]
allGroups = concat . takeWhile (not . null) . iterate (concatMap (concat . gsNested))

-- | Replace the given line ranges with empty lines, keeping every other line
-- where it was.
blanking :: [(Int, Int)] -> Text -> Text
blanking ranges = T.unlines . go (sortOn fst ranges) . zip [1 ..] . T.lines
  where
    go rs = \case
      [] -> []
      (n, l) : ls -> case dropWhile ((< n) . snd) rs of
        rs'@((from, _) : _) | from <= n -> "" : go rs' ls
        rs' -> l : go rs' ls

-- | The lines to blank so that configuration @i@ of a group is what is left.
--
-- The branches this configuration does not take, and the directives
-- themselves: a directive belongs to no configuration, which is the whole of
-- what separates this from 'droppedFor'.
blankingFor :: GroupSpec -> Int -> [(Int, Int)]
blankingFor gs i = droppedFor gs i <> gsOwnRanges gs

-- | The lines a configuration of a group is not including.
droppedFor :: GroupSpec -> Int -> [(Int, Int)]
droppedFor gs i
  | i < length (gsGuards gs) || gsHasElse gs =
      [r | (k, r) <- zip [0 :: Int ..] (gsBranches gs), k /= i]
  | otherwise = gsBranches gs

-- | The directives that do not introduce configurations.
opaqueDirectives :: Text -> [Directive]
opaqueDirectives = filter ((`elem` opaqueKeywords) . dKeyword) . directives
