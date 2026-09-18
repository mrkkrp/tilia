{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Putting a module's imports in order.
module Tilia.Imports
  ( normalizeImports,
  )
where

import Data.Char (isAlphaNum)
import Data.Choice (Choice, isTrue)
import Data.Function (on, (&))
import Data.List (groupBy, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (unpackFS)
import GHC.Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SourceText (StringLiteral (..))
import GHC.Types.SrcLoc
import Tilia.Comments (Comment (..), commentTrailing, commentsWithin)
import Tilia.Span (endPoint, startPoint)
import Tilia.Span.Ghc (spanOf, spanOfSrcSpan)

-- | Whether an explicit @import Prelude@ is telling the reader anything.
data PreludeImport
  = -- | @ImplicitPrelude@ is on, so the module has the Prelude whatever it
    -- says, and the line only trims what it already takes.
    Refines
  | -- | @ImplicitPrelude@ is off, so the line is the only reason the module
    -- has a Prelude at all, and it is an import like any other.
    Provides
  deriving (Eq, Show)

-- | Sort a module's imports and fold together the ones that say the same
-- thing.
normalizeImports ::
  -- | Whether @ImplicitPrelude@ is on.
  Choice "implicitPrelude" ->
  -- | Source lines the block must not be sorted across.
  [Int] ->
  -- | The module's comments.
  [Comment] ->
  -- | Original imports.
  [LImportDecl GhcPs] ->
  -- | Normalized imports.
  [LImportDecl GhcPs]
normalizeImports implicitPrelude barriers written imports =
  concatMap stretch (segmented (dividing imports barriers) tidied)
  where
    prelude = if isTrue implicitPrelude then Refines else Provides
    tidied = map (fmap (tidyList written)) imports
    stretch is = foldRuns (fuse written) [((identity prelude i, alone i), i) | i <- is]
    alone i
      | any strands (spanOf i) = startLineOf i
      | otherwise = 0
      where
        strands s =
          any (unanchored (itemStarts i)) (filter loose (commentsWithin s written))
        loose = not . commentTrailing
    unanchored starts c = not (any (> endPoint (commentSpan c)) starts)
    startLineOf i = case srcSpanStart (getLocA i) of
      RealSrcLoc l _ -> srcLocLine l
      _ -> 0

-- | Where every name an import lists begins, the names inside a thing's own
-- brackets among them.
itemStarts :: LImportDecl GhcPs -> [(Int, Int)]
itemStarts (L _ decl) = case ideclImportList decl of
  Nothing -> []
  Just (_, L _ items) -> concatMap starts items
  where
    starts item = foldMap ((: []) . startPoint) (spanOf item) <> inside (unLoc item)
    inside = \case
      IEThingWith _ _ _ members _ ->
        concatMap (foldMap ((: []) . startPoint) . spanOf) members
      _ -> []

-- | The lines that fall between imports, out of the lines that must not be
-- sorted across.
dividing :: [LImportDecl GhcPs] -> [Int] -> [Int]
dividing imports = filter (not . within)
  where
    within l = any (\(from, to) -> from <= l && l <= to) spans'
    spans' = [(srcLocLine from, srcLocLine to) | i <- imports, Just (from, to) <- [endsOf i]]
    endsOf i = case (srcSpanStart (getLocA i), srcSpanEnd (getLocA i)) of
      (RealSrcLoc from _, RealSrcLoc to _) -> Just (from, to)
      _ -> Nothing

-- | Cut a list of imports into the stretches the barriers leave between
-- them, in order.
segmented :: [Int] -> [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
segmented [] imports = [imports]
segmented barriers imports =
  groupBy ((==) `on` fst) [(between i, i) | i <- imports] & map (map snd)
  where
    between i = length (takeWhile (< lineOf i) barriers)
    lineOf i = case srcSpanStart (getLocA i) of
      RealSrcLoc l _ -> srcLocLine l
      _ -> 0

----------------------------------------------------------------------------
-- Runs

-- | Sort by the keys, then replace each run of equal keys by one value
-- folded out of it.
--
-- The sort is stable, so a run holds its values in the order they were
-- written and the fold sees them that way round. That is worth having:
-- folding keeps the first one's identity, and \"first\" should mean first
-- in the file.
foldRuns :: (Ord k) => (a -> a -> a) -> [(k, a)] -> [a]
foldRuns fold' =
  map (foldl1 fold' . map snd) . groupBy ((==) `on` fst) . sortOn fst

----------------------------------------------------------------------------
-- Which imports are the same import

-- | What has to agree before two imports may be folded together, in the
-- order imports should be printed in.
--
-- The two leading keys are about reading rather than about identity. A
-- @Prelude@ that only refines what the module already has goes at the end,
-- since looking for it among the @D@s would be looking for the least
-- interesting line in the block. The package goes before the module name so
-- that the imports from one package stay in one run; sorting by module
-- first would interleave them and hide who provides what.
identity ::
  PreludeImport ->
  LImportDecl GhcPs ->
  (Bool, (Int, Text), Text, Bool, Bool, Bool, Maybe Text, Maybe Bool, Maybe Bool)
identity prelude (L _ decl) =
  ( prelude == Refines && named (ideclName decl) == T.pack "Prelude",
    package (ideclPkgQual decl),
    named (ideclName decl),
    ideclSource decl == IsBoot,
    ideclSafe decl,
    isImportDeclQualified (ideclQualified decl),
    named <$> ideclAs decl,
    hides . fst <$> ideclImportList decl,
    lifted (ideclLevelSpec decl)
  )
  where
    named = T.pack . moduleNameString . unLoc
    package = \case
      NoRawPkgQual -> (0, T.empty)
      RawPkgQual (sl_fs -> fs)
        | name == T.pack "this" -> (2, T.empty)
        | otherwise -> (1, name)
        where
          name = T.pack (unpackFS fs)
    hides = \case
      Exactly -> False
      EverythingBut -> True
    lifted = \case
      NotLevelled -> Nothing
      LevelStylePre l -> Just (quoted l)
      LevelStylePost l -> Just (quoted l)
    quoted = \case
      ImportDeclSplice -> False
      ImportDeclQuote -> True

----------------------------------------------------------------------------
-- Folding two imports into one

-- | Keep the first import and give it everything the second named.
--
-- The result covers both of their spans. That matters for comments: one
-- written between the two has to land inside the declaration that replaces
-- them, and a folded import claiming only the first one's span would leave
-- it nowhere to go.
fuse :: [Comment] -> LImportDecl GhcPs -> LImportDecl GhcPs -> LImportDecl GhcPs
fuse written (L ann kept) (L other folded) =
  L
    ann {entry = EpaSpan (combineSrcSpans (locA ann) (locA other))}
    kept {ideclImportList = both (ideclImportList kept) (ideclImportList folded)}
  where
    both (Just (interpretation, L l xs)) (Just (_, L l' ys)) =
      Just (interpretation, L (widened written l l') (tidyItems written (xs <> ys)))
    both _ _ = Nothing

----------------------------------------------------------------------------
-- The names inside an import list

tidyList :: [Comment] -> ImportDecl GhcPs -> ImportDecl GhcPs
tidyList written decl =
  decl {ideclImportList = fmap (fmap (tidyItems written)) <$> ideclImportList decl}

-- | Sort an import list and fold together the entries naming one thing.
--
-- @import M (T (A), T (B))@ names one type twice and comes out as @import M
-- (T (A, B))@.
tidyItems :: [Comment] -> [LIE GhcPs] -> [LIE GhcPs]
tidyItems written items
  -- An import list should hold nothing but names, and the parser will accept
  -- things there that the compiler goes on to reject—@import M (module N)@
  -- among them. Sorting a list we cannot read would be guessing.
  | any (unnameable . unLoc) items = items
  | otherwise = foldRuns (wider written) [(nameOf (unLoc i), fmap sortSubnames i) | i <- items]
  where
    unnameable = \case
      IEVar {} -> False
      IEThingAbs {} -> False
      IEThingAll {} -> False
      IEThingWith {} -> False
      _ -> True

-- | Cover both of these regions, if anything was written between them.
--
-- A region says two things at once: where a comment written inside it
-- belongs, and how the construct was laid out. What comes out of folding was
-- never written, so it has no layout of its own, and taking the region that
-- covers everything folded in would have it laid out across all the lines
-- those names were spread over—several lines for a name or two.
--
-- So the region grows only where growing it is the point: when a comment
-- falls between the two, and would otherwise be left outside the entry that
-- now holds the names it was written among.
widened :: [Comment] -> EpAnn ann -> EpAnn ann -> EpAnn ann
widened written a b
  | any holdsComment (spanOfSrcSpan combined) = a {entry = EpaSpan combined}
  | otherwise = a
  where
    combined = combineSrcSpans (locA a) (locA b)
    holdsComment s = not (null (commentsWithin s written))

-- | Of two entries naming one thing, the one that brings in more of it.
--
-- Naming all of a type beats naming some of its pieces, which beats naming
-- the type alone. Where both name some, the two lists go together. The
-- documentation is dropped whenever two entries are folded: it was written
-- against one of them and would become a claim about both.
wider :: [Comment] -> LIE GhcPs -> LIE GhcPs -> LIE GhcPs
wider written (L ann kept) (L other folded) =
  L (widened written ann other) (combine kept folded)
  where
    combine a b = case (a, b) of
      (IEThingAll x n _, _) -> IEThingAll x n Nothing
      (_, IEThingAll x n _) -> IEThingAll x n Nothing
      (IEThingWith x n wildcard subs _, IEThingWith _ _ wildcard' subs' _) ->
        IEThingWith
          x
          n
          (eitherWildcard wildcard wildcard')
          (dedupeSubnames (subs <> subs'))
          Nothing
      (IEThingWith x n wildcard subs _, _) ->
        IEThingWith x n wildcard subs Nothing
      (_, IEThingWith x n wildcard subs _) ->
        IEThingWith x n wildcard subs Nothing
      (IEVar _ n _, _) -> IEVar Nothing n Nothing
      _ -> a

    eitherWildcard a b = case (a, b) of
      (NoIEWildcard, NoIEWildcard) -> NoIEWildcard
      _ -> IEWildcard 0

sortSubnames :: IE GhcPs -> IE GhcPs
sortSubnames = \case
  IEThingWith x n wildcard subs doc ->
    IEThingWith x n wildcard (dedupeSubnames subs) doc
  other -> other

-- | The same name written twice in a sub-list is written once here. Which
-- of the two survives cannot matter: they name the same thing.
dedupeSubnames :: [LIEWrappedName GhcPs] -> [LIEWrappedName GhcPs]
dedupeSubnames subs = foldRuns const [(nameKey (unLoc s), s) | s <- subs]

----------------------------------------------------------------------------
-- Ordering names

nameOf :: IE GhcPs -> (Int, Bool, String)
nameOf = \case
  IEVar _ x _ -> nameKey (unLoc x)
  IEThingAbs _ x _ -> nameKey (unLoc x)
  IEThingAll _ x _ -> nameKey (unLoc x)
  IEThingWith _ x _ _ _ -> nameKey (unLoc x)
  -- 'tidyItems' has already refused to touch a list holding anything else.
  _ -> (maxBound, True, "")

-- | Where a name sorts.
--
-- Grouped first by what kind of thing is named, so that the @pattern@s and
-- the @type@s of an import list stay together. Then the names spelled with
-- letters before the ones spelled with punctuation, which gathers the
-- operators at the end where they are easy to find; ordering by character
-- code would scatter them, some before the letters and some after.
nameKey :: IEWrappedName GhcPs -> (Int, Bool, String)
nameKey = \case
  IEName _ x -> spelled 0 x
  IEDefault _ x -> spelled 1 x
  IEPattern _ x -> spelled 2 x
  IEType _ x -> spelled 3 x
  IEData _ x -> spelled 4 x
  where
    spelled :: Int -> LocatedN RdrName -> (Int, Bool, String)
    spelled kind (unLoc -> name) = (kind, punctuation text, text)
      where
        text = occNameString (rdrNameOcc name)
    punctuation = \case
      (c : _) -> not (isAlphaNum c)
      [] -> False
