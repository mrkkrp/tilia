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

-- | Sort and fold together a module's imports.
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
  concatMap stretch (cutAtBarriers (barriersBetween imports barriers) tidied)
  where
    tidied = fmap (fmap (tidyImportList written)) imports
    stretch is =
      foldRuns
        (combineImports written)
        [((importIdentity implicitPrelude i, alone i), i) | i <- is]
    alone i
      | any strands (spanOf i) = startLineOf i
      | otherwise = 0
      where
        strands s =
          any
            (unanchored (itemStarts i))
            (filter loose (commentsWithin s written))
        loose = not . commentTrailing
    unanchored starts c = not (any (> endPoint (commentSpan c)) starts)
    startLineOf i = case srcSpanStart (getLocA i) of
      RealSrcLoc l _ -> srcLocLine l
      _ -> 0

-- | Where every name an import lists begins, each as a line and a column.
--
-- The names inside a thing's own brackets count too, so @T (A, B)@ gives
-- three positions: the start of @T@, of @A@ and of @B@. An import with no
-- list has none at all.
itemStarts :: LImportDecl GhcPs -> [(Int, Int)]
itemStarts (L _ decl) = case ideclImportList decl of
  Nothing -> []
  Just (_, L _ items) -> concatMap starts items
  where
    starts item =
      foldMap ((: []) . startPoint) (spanOf item)
        <> inside (unLoc item)
    inside = \case
      IEThingWith _ _ _ members _ ->
        concatMap (foldMap ((: []) . startPoint) . spanOf) members
      _ -> []

-- | The barriers that fall between two imports rather than inside one.
barriersBetween ::
  -- | The imports as written, for the lines each of them covers.
  [LImportDecl GhcPs] ->
  -- | Lines the block must not be sorted across.
  [Int] ->
  -- | Those of them that lie between imports.
  [Int]
barriersBetween imports = filter (not . within)
  where
    within l = any (\(from, to) -> from <= l && l <= to) spans'
    spans' =
      [ (srcLocLine from, srcLocLine to)
      | i <- imports,
        Just (from, to) <- [endsOf i]
      ]
    endsOf i = case (srcSpanStart (getLocA i), srcSpanEnd (getLocA i)) of
      (RealSrcLoc from _, RealSrcLoc to _) -> Just (from, to)
      _ -> Nothing

-- | Cut the imports into the stretches the barriers leave between them.
cutAtBarriers ::
  -- | Lines the block must not be sorted across, in ascending order.
  [Int] ->
  -- | The imports, as they were written.
  [LImportDecl GhcPs] ->
  -- | One stretch per run of imports between barriers, in the same order.
  [[LImportDecl GhcPs]]
cutAtBarriers [] imports = [imports]
cutAtBarriers barriers imports =
  groupBy ((==) `on` fst) [(between i, i) | i <- imports] & fmap (fmap snd)
  where
    between i = length (takeWhile (< lineOf i) barriers)
    lineOf i = case srcSpanStart (getLocA i) of
      RealSrcLoc l _ -> srcLocLine l
      _ -> 0

-- | Sort by the keys, then replace each run of equal keys by one value
-- folded out of it.
foldRuns ::
  (Ord k) =>
  -- | How to fold two values that share a key.
  (a -> a -> a) ->
  -- | The values, each under the key it sorts and folds by.
  [(k, a)] ->
  -- | One value per distinct key, in key order.
  [a]
foldRuns fold' =
  fmap (foldl1 fold' . fmap snd) . groupBy ((==) `on` fst) . sortOn fst

-- | What has to agree before two imports may be folded together.
--
-- The derived ordering is the whole of the policy: the fields are compared
-- in the order they are declared, which is the order imports are printed
-- in. The two leading fields are about reading rather than about identity.
-- A @Prelude@ that only refines what the module already has goes at the
-- end, since looking for it among the @D@s would be looking for the least
-- interesting line in the block. The package goes before the module name so
-- that the imports from one package stay in one run; sorting by module
-- first would interleave them and hide who provides what.
data ImportIdentity = ImportIdentity
  { -- | A @Prelude@ that only refines what the module already has.
    iiRefinesPrelude :: Bool,
    -- | The package it names, where it names one.
    iiPackage :: (Int, Text),
    -- | The module it brings in.
    iiModule :: Text,
    -- | Written @{-\# SOURCE \#-}@?
    iiBoot :: Bool,
    -- | Written @safe@?
    iiSafe :: Bool,
    -- | Written @qualified@?
    iiQualified :: Bool,
    -- | The name it was given with @as@, where it was given one.
    iiAlias :: Maybe Text,
    -- | Whether its list hides rather than names, where it has a list.
    iiHides :: Maybe Bool,
    -- | Whether it is a @quote@ rather than a @splice@ import, where it is
    -- levelled at all.
    iiLevel :: Maybe Bool
  }
  deriving (Eq, Ord, Show)

-- | The identity of one import.
importIdentity ::
  -- | Whether @ImplicitPrelude@ is on.
  Choice "implicitPrelude" ->
  -- | The import.
  LImportDecl GhcPs ->
  ImportIdentity
importIdentity implicitPrelude (L _ decl) =
  ImportIdentity
    { iiRefinesPrelude =
        isTrue implicitPrelude && named (ideclName decl) == T.pack "Prelude",
      iiPackage = package (ideclPkgQual decl),
      iiModule = named (ideclName decl),
      iiBoot = ideclSource decl == IsBoot,
      iiSafe = ideclSafe decl,
      iiQualified = isImportDeclQualified (ideclQualified decl),
      iiAlias = named <$> ideclAs decl,
      iiHides = hides . fst <$> ideclImportList decl,
      iiLevel = lifted (ideclLevelSpec decl)
    }
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

-- | Keep the first import and give it everything the second named.
combineImports ::
  -- | The module's comments, which decide how far the span may grow.
  [Comment] ->
  -- | The import kept, whose spelling the result takes.
  LImportDecl GhcPs ->
  -- | The import folded in, which gives up its list and its span.
  LImportDecl GhcPs ->
  -- | The first, listing what both named, across both their spans.
  LImportDecl GhcPs
combineImports written (L ann kept) (L other folded) =
  L
    ann{entry = EpaSpan (combineSrcSpans (locA ann) (locA other))}
    kept{ideclImportList = both (ideclImportList kept) (ideclImportList folded)}
  where
    both (Just (interpretation, L l xs)) (Just (_, L l' ys)) =
      Just (interpretation, L (widened written l l') (tidyImportItems written (xs <> ys)))
    both _ _ = Nothing

-- | An import with its list sorted and the entries naming one thing folded
-- together. An import with no list is left as it is.
tidyImportList :: [Comment] -> ImportDecl GhcPs -> ImportDecl GhcPs
tidyImportList written decl =
  decl
    { ideclImportList =
        fmap (fmap (tidyImportItems written)) <$> ideclImportList decl
    }

-- | Sort an import list and fold together the entries naming one thing.
--
-- @import M (T (A), T (B))@ names one type twice and comes out as @import M
-- (T (A, B))@.
tidyImportItems :: [Comment] -> [LIE GhcPs] -> [LIE GhcPs]
tidyImportItems written items
  | any (unnameable . unLoc) items = items
  | otherwise =
      foldRuns
        (wider written)
        [(nameIdentity (unLoc i), fmap sortSubnames i) | i <- items]
  where
    unnameable = \case
      IEVar{} -> False
      IEThingAbs{} -> False
      IEThingAll{} -> False
      IEThingWith{} -> False
      _ -> True

-- | Cover both of these regions, if anything was written between them.
widened :: [Comment] -> EpAnn ann -> EpAnn ann -> EpAnn ann
widened written a b
  | any holdsComment (spanOfSrcSpan combined) = a{entry = EpaSpan combined}
  | otherwise = a
  where
    combined = combineSrcSpans (locA a) (locA b)
    holdsComment s = not (null (commentsWithin s written))

-- | Of two entries naming one thing, the one that brings in more of it.
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

-- | An entry with the names in its own brackets sorted and deduplicated.
sortSubnames :: IE GhcPs -> IE GhcPs
sortSubnames = \case
  IEThingWith x n wildcard subs doc ->
    IEThingWith x n wildcard (dedupeSubnames subs) doc
  other -> other

-- | Deduplicate names in sub-lists.
dedupeSubnames :: [LIEWrappedName GhcPs] -> [LIEWrappedName GhcPs]
dedupeSubnames subs = foldRuns const [(wrappedNameIdentity (unLoc s), s) | s <- subs]

-- | Where a name sorts, and what makes two of them name one thing.
--
-- The derived ordering is the whole of the policy: the fields are compared
-- in the order they are declared. Names are grouped first by what kind of
-- thing is named, so that the @pattern@s and the @type@s of an import list
-- stay together. Then the ones spelled with letters come before the ones
-- spelled with punctuation, which gathers the operators at the end where
-- they are easy to find; ordering by character code would scatter them,
-- some before the letters and some after.
data NameIdentity = NameIdentity
  { -- | Which kind of thing is named: a plain name, then a @default@, a
    -- @pattern@, a @type@ and a @data@.
    niKind :: Int,
    -- | Is it spelled with punctuation rather than letters?
    niPunctuation :: Bool,
    -- | The name as it was written.
    niName :: String
  }
  deriving (Eq, Ord, Show)

-- | The identity of the name an entry is about.
nameIdentity :: IE GhcPs -> NameIdentity
nameIdentity = \case
  IEVar _ x _ -> wrappedNameIdentity (unLoc x)
  IEThingAbs _ x _ -> wrappedNameIdentity (unLoc x)
  IEThingAll _ x _ -> wrappedNameIdentity (unLoc x)
  IEThingWith _ x _ _ _ -> wrappedNameIdentity (unLoc x)
  _ ->
    NameIdentity
      { niKind = maxBound,
        niPunctuation = True,
        niName = ""
      }

-- | The identity of one name, as an import list writes it.
wrappedNameIdentity :: IEWrappedName GhcPs -> NameIdentity
wrappedNameIdentity = \case
  IEName _ x -> spelled 0 x
  IEDefault _ x -> spelled 1 x
  IEPattern _ x -> spelled 2 x
  IEType _ x -> spelled 3 x
  IEData _ x -> spelled 4 x
  where
    spelled :: Int -> LocatedN RdrName -> NameIdentity
    spelled kind (unLoc -> name) =
      NameIdentity
        { niKind = kind,
          niPunctuation = punctuation text,
          niName = text
        }
      where
        text = occNameString (rdrNameOcc name)
    punctuation = \case
      (c : _) -> not (isAlphaNum c)
      [] -> False
