{-# LANGUAGE LambdaCase #-}

-- | Formatting the declarations a conditional reaches rather than the whole
-- module around them.
module Tilia.Cpp.Fragment
  ( Body,
    bodyOf,
    Fragment,
    fragmentsOf,
    fragmentText,
    reassembled,
    linesHeld,
  )
where

import Control.Monad (guard)
import Data.List (partition, sortOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Any (..))
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Cpp.Directives
  ( GroupSpec (..),
    allGroups,
    blanking,
    blankingFor,
    droppedFor,
  )
import Tilia.Doc.Internal
  ( Doc (..),
    Layout (..),
    foldChildren,
    printedFrom,
    spine,
    spineAt,
  )

-- | A module's document, taken apart where its declarations begin.
data Body = Body
  { -- | The last line before the first run of declarations.
    bodyHeadEnd :: !Int,
    -- | Whether anything above the declarations is a choice.
    bodyHeadVaries :: !Bool,
    -- | How the declarations are laid out.
    bodyLayout :: !Layout,
    -- | The declarations, as the elements of that layout.
    bodyItems :: [Doc],
    -- | The runs of declarations the printer keeps together, in order.
    bodyRuns :: [Run],
    -- | The document with other declarations in place of these.
    bodyWith :: Doc -> Doc
  }

-- | Declarations the printer keeps together, with no empty line between
-- them.
data Run = Run
  { -- | The first element of the body that was printed from something.
    runFirst :: !Int,
    -- | The last one.
    runLast :: !Int,
    -- | The first line it was printed from.
    runFrom :: !Int,
    -- | The last one.
    runTo :: !Int
  }

-- | Find the declarations in a module's document.
bodyOf :: Doc -> Maybe Body
bodyOf doc = do
  (DGroup headerLayout inside, around) <- lastOf doc
  (DGroup layout content, within) <- lastOf inside
  let with decls = around (DGroup headerLayout (within decls))
      items = spineAt layout content
  runs <- runsOf items
  pure
    Body
      { bodyHeadEnd = case runs of
          r : _ -> runFrom r - 1
          [] -> maximum (0 : fmap snd (printedFrom doc)),
        bodyHeadVaries = choosing (with mempty),
        bodyLayout = layout,
        bodyItems = items,
        bodyRuns = runs,
        bodyWith = with
      }

-- | The last element of a document's spine, and the document with another
-- in its place.
lastOf :: Doc -> Maybe (Doc, Doc -> Doc)
lastOf = \case
  DCat a b
    | null (spine b) -> (\(x, plug) -> (x, (`DCat` b) . plug)) <$> lastOf a
    | otherwise -> (\(x, plug) -> (x, DCat a . plug)) <$> lastOf b
  DEmpty -> Nothing
  d -> Just (d, id)

-- | Cut the elements of a body into runs at the empty lines between them.
runsOf :: [Doc] -> Maybe [Run]
runsOf items =
  ordered (mapMaybe asRun (pieces (zip [0 ..] items)))
  where
    pieces xs = case break ((== DBreak) . snd) xs of
      (piece, []) -> [piece]
      (piece, _ : rest) -> piece : pieces rest
    asRun piece = case [(i, e) | (i, d) <- piece, Just e <- [extent d]] of
      [] -> Nothing
      located@((i, _) : _) ->
        Just
          Run
            { runFirst = i,
              runLast = fst (last located),
              runFrom = minimum (fmap (fst . snd) located),
              runTo = maximum (fmap (snd . snd) located)
            }
    ordered runs
      | and (zipWith (\a b -> runTo a < runFrom b) runs (drop 1 runs)) = Just runs
      | otherwise = Nothing

-- | The conditionals that reach one fragment of a module.
data Fragment = Fragment
  { -- | The conditionals, outermost ones only.
    fragmentGroups :: [GroupSpec],
    -- | Whether it takes in what is above the declarations.
    fragmentAbove :: !Bool,
    -- | The run before it, if there is one.
    fragmentBefore :: !(Maybe Run),
    -- | The run after it, if there is one.
    fragmentAfter :: !(Maybe Run)
  }

-- | Split a module's outermost conditionals into fragments that can be
-- formatted apart, or 'Nothing' where they cannot be.
fragmentsOf :: Body -> [GroupSpec] -> Maybe [Fragment]
fragmentsOf body forest = do
  guard (bodyLayout body == Broken)
  let (above, below) = partition ((<= bodyHeadEnd body) . fst . gsWhole) forest
  guard (all ((<= bodyHeadEnd body) . snd . gsWhole) above)
  let fragment gs isAbove (lo, hi) = Fragment gs isAbove (runAt (lo - 1)) (runAt hi)
      runAt i
        | i < 0 = Nothing
        | otherwise = listToMaybe (drop i (bodyRuns body))
  pure $
    case ( above,
           clustered
             (sortOn (fst . snd) [(g, reach (bodyRuns body) (gsWhole g)) | g <- below])
         ) of
      ([], apart) -> [fragment gs False r | (gs, r) <- apart]
      (_, (gs, r@(0, _)) : rest) ->
        fragment (above <> gs) True r : [fragment gs' False r' | (gs', r') <- rest]
      (_, apart) -> fragment above True (0, 0) : [fragment gs False r | (gs, r) <- apart]

-- | The runs a conditional's lines touch, or where it falls between two of
-- them.
reach :: [Run] -> (Int, Int) -> (Int, Int)
reach runs (a, b) =
  case [i | (i, r) <- zip [0 ..] runs, runFrom r <= b, a <= runTo r] of
    [] -> let p = length (takeWhile ((< a) . runTo) runs) in (p, p)
    is -> (minimum is, maximum is + 1)

-- | Join conditionals whose fragments would overlap once the runs either
-- side of them are taken along.
clustered :: [(GroupSpec, (Int, Int))] -> [([GroupSpec], (Int, Int))]
clustered = \case
  [] -> []
  (g, r) : rest -> go [g] r rest
  where
    go gs (lo, hi) = \case
      (g, (lo', hi')) : rest | lo' <= hi -> go (gs <> [g]) (lo, max hi hi') rest
      rest -> (gs, (lo, hi)) : clustered rest

-- | The text a fragment is formatted from, with the lines it leaves out.
--
-- It keeps what is above the declarations, the fragment, and a run either
-- side of it, and takes the first branch of every other conditional. What
-- it leaves out above the fragment is blanked, so that every line stays
-- where it was written, and what it leaves out below is cut off. The text
-- is 'Nothing' where that leaves out nothing.
fragmentText ::
  Body ->
  -- | The module's outermost conditionals.
  [GroupSpec] ->
  -- | The module.
  Text ->
  Fragment ->
  Maybe (Text, [(Int, Int)])
fragmentText body forest source f
  | linesHeld text < linesHeld source = Just (text, outside <> concatMap (`droppedFor` 0) others)
  | otherwise = Nothing
  where
    text =
      T.unlines . take to . T.lines $
        blanking (outside <> concatMap (`blankingFor` 0) others) source
    lineCount = length (T.lines source)
    headEnd = bodyHeadEnd body
    from = maybe (headEnd + 1) runFrom (fragmentBefore f)
    to = maybe lineCount runTo (fragmentAfter f)
    outside =
      [(headEnd + 1, from - 1) | not (fragmentAbove f), from > headEnd + 1]
        <> [(to + 1, lineCount) | to < lineCount]
    others =
      allGroups
        [ g
        | g <- forest,
          gsWhole g `notElem` fmap gsWhole (fragmentGroups f)
        ]

-- | Put what each fragment was formatted to in place of what the module's
-- body holds there, or 'Nothing' where one of them came out other than a
-- fragment of declarations.
reassembled ::
  Body ->
  -- | Each fragment, with what it was formatted to.
  [(Fragment, Doc)] ->
  Maybe Doc
reassembled body formatted = do
  placed <- traverse place formatted
  with <- case [b | (f, b, _) <- placed, fragmentAbove f] of
    [] -> Just (bodyWith body)
    [b] -> Just (bodyWith b)
    _ -> Nothing
  let items = foldr put (bodyItems body) (sortOn (\(i, _, _) -> i) [p | (_, _, p) <- placed])
  pure (with (DGroup (bodyLayout body) (mconcat items)))
  where
    put (from, to, new) items = take from items <> new <> drop to items

    -- The fragment, the body of what it was formatted to, and the
    -- elements to put in place of the module's, with where they go.
    place (f, d) = do
      b <- bodyOf d
      guard (fragmentAbove f || not (bodyHeadVaries b))
      let indexed = zip [0 :: Int ..] (bodyItems b)
          within r x = maybe False (\(s, e) -> runFrom r <= s && e <= runTo r) (extent x)
      start <- case fragmentBefore f of
        Nothing -> Just (-1)
        Just r -> listToMaybe (reverse [i | (i, x) <- indexed, within r x])
      end <- case fragmentAfter f of
        Nothing -> Just (length indexed)
        Just r -> listToMaybe [i | (i, x) <- indexed, within r x]
      let new = [x | (i, x) <- indexed, start < i, i < end]
          low
            | fragmentAbove f = 0
            | otherwise = maybe (bodyHeadEnd body) runTo (fragmentBefore f)
          high = maybe maxBound runFrom (fragmentAfter f)
      guard (start < end)
      guard (all (maybe True (\(s, e) -> low < s && e < high) . extent) new)
      pure
        ( f,
          b,
          ( maybe 0 ((+ 1) . runLast) (fragmentBefore f),
            maybe (length (bodyItems body)) runFirst (fragmentAfter f),
            new
          )
        )

-- | How many lines of a text hold anything, which is what formatting it
-- costs.
linesHeld :: Text -> Int
linesHeld = length . filter (not . T.null . T.strip) . T.lines

-- | The first and the last line a document was printed from.
extent :: Doc -> Maybe (Int, Int)
extent d = case printedFrom d of
  [] -> Nothing
  ls -> Just (minimum (fmap fst ls), maximum (fmap snd ls))

-- | Is there a choice anywhere in this document?
choosing :: Doc -> Bool
choosing = getAny . go
  where
    go = \case
      DCppChoice{} -> Any True
      d -> foldChildren go d
