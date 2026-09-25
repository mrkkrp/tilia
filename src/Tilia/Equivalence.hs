{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Whether formatting changed what a module says.
module Tilia.Equivalence
  ( syntaxDifference,
    commentDifference,
  )
where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Choice (pattern Is)
import Data.Data
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, isNothing, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (FastString)
import GHC.Hs (HsModule (..), XModulePs (..))
import GHC.Hs.Decls (DerivClauseTys (..), DocDecl (..), HsDecl (..), LHsDecl)
import GHC.Hs.Doc (LHsDoc, WithHsDocIdentifiers (..))
import GHC.Hs.DocString
  ( HsDocString (..),
    HsDocStringChunk (..),
    HsDocStringDecorator (..),
  )
import GHC.Hs.Expr (HsExpr (..), LHsExpr)
import GHC.Hs.Extension (GhcPs)
import GHC.Hs.ImpExp
  ( IE (..),
    ImportDeclQualifiedStyle,
    LIE,
    LImportDecl,
    isImportDeclQualified,
  )
import GHC.Hs.Type (HsType (..), LHsContext, LHsSigType)
import GHC.Types.Name (Name)
import GHC.Types.Name.Occurrence (OccName)
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Types (Unit)
import Language.Haskell.Syntax.Extension (XRec)
import Language.Haskell.Syntax.Module.Name (ModuleName)
import System.IO.Unsafe (unsafePerformIO)
import Tilia.Comments
  ( Comment (..),
    CommentStyle (..),
    Pragma (..),
    commentPragma,
    commentTrailing,
    escapeTrigger,
    triggerEscaped,
  )
import Tilia.Imports (normalizeImports)
import Tilia.Span (Span (..))
import Tilia.Span.Ghc (spanOf, spansOf)
import Tilia.Utils (tshow)

----------------------------------------------------------------------------
-- Syntax

-- | Where two ASTs start to differ.
syntaxDifference :: (Data a) => a -> a -> Maybe Text
syntaxDifference = differ []

-- | The constructors on the way down to where the walk has got to,
-- innermost first.
type Path = [Constr]

differ :: forall a. (Data a) => Path -> a -> a -> Maybe Text
differ path x y = case classify (typeOf x) of
  Incidental -> Nothing
  Special
    | Just outcome <- asStandaloneDoc path x y -> outcome
    | Just outcome <- asExportItems path x y -> outcome
    | Just outcome <- asDeclarations path x y -> outcome
    | Just outcome <- asDerivingClause path x y -> outcome
    | Just outcome <- asQualifiedStyle path x y -> outcome
    | Just outcome <- asDocString path x y -> outcome
    | Just outcome <- asContext path x y -> outcome
    | Just outcome <- asImports path x y -> outcome
    | otherwise -> structurally
  Ordinary -> structurally
  where
    structurally = case dataTypeRep (dataTypeOf x) of
      AlgRep _
        | toConstr x /= toConstr y -> Just disagreement
        | settledByConstructor (toConstr x) -> Nothing
        | otherwise ->
            firstOf
              ( zipWith
                  (cellDiffer (toConstr x : path))
                  (gmapQ Cell x)
                  (gmapQ Cell y)
              )
      NoRep
        | opaque x y -> Nothing
        | otherwise ->
            Just (describe path (T.pack (typeNameOf x) <> " changed"))
      _
        | toConstr x == toConstr y -> Nothing
        | otherwise -> Just disagreement
    disagreement =
      describe path (named (toConstr x) <> " became " <> named (toConstr y))

-- | What is known about a type before either value of it is looked at.
data Verdict
  = -- | Records only how or where something was written. See 'incidental'.
    Incidental
  | -- | One of the types the @as…@ functions below compare by hand.
    Special
  | -- | Compared by its constructor and then field by field.
    Ordinary

-- | Which of the three a type is, worked out once.
classify :: TypeRep -> Verdict
classify rep = unsafePerformIO $ do
  known <- readIORef classified
  case Map.lookup rep known of
    Just verdict -> pure verdict
    Nothing -> do
      let verdict = worked
      atomicModifyIORef' classified (\m -> (Map.insert rep verdict m, ()))
      pure verdict
  where
    worked
      | incidental rep = Incidental
      | rep `Set.member` comparedByHand = Special
      | otherwise = Ordinary

classified :: IORef (Map TypeRep Verdict)
classified = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE classified #-}

-- | The types compared by hand, which is to say the ones the @as…@ chain in
-- 'differ' can match.
comparedByHand :: Set TypeRep
comparedByHand =
  Set.fromList
    [ typeRep (Proxy @(Maybe (LHsDoc GhcPs))),
      typeRep (Proxy @[LIE GhcPs]),
      typeRep (Proxy @[LHsDecl GhcPs]),
      typeRep (Proxy @(DerivClauseTys GhcPs)),
      typeRep (Proxy @ImportDeclQualifiedStyle),
      typeRep (Proxy @HsDocString),
      typeRep (Proxy @(Maybe (LHsContext GhcPs))),
      typeRep (Proxy @(LHsContext GhcPs)),
      typeRep (Proxy @(XRec GhcPs [LHsExpr GhcPs])),
      typeRep (Proxy @[LImportDecl GhcPs])
    ]

-- | Constructors whose fields say only how they were written.
--
-- @HsStarTy@ carries a flag for whether the @*@ was typed as @★@. Both are
-- the same kind; which one the author reached for is spelling.
settledByConstructor :: Constr -> Bool
settledByConstructor c = showConstr c == "HsStarTy"

named :: Constr -> Text
named = T.pack . showConstr

-- | The tail of the path, and what was found at the end of it.
describe :: Path -> Text -> Text
describe path leaf =
  T.intercalate " > " (fmap named (reverse (take 5 path)) <> [leaf])

firstOf :: [Maybe a] -> Maybe a
firstOf = listToMaybe . catMaybes

-- | One field of a value, with its type hidden.
data Cell = forall d. (Data d) => Cell d

cellDiffer :: Path -> Cell -> Cell -> Maybe Text
cellDiffer path (Cell a) (Cell b) = case cast b of
  Just b' -> differ path a b'
  Nothing -> Just (describe path "fields of different types")

-- | Two lists compared one element at a time.
--
-- Handed back to 'differ' whole they would arrive at the same @as…@ function
-- again and never stop, which is why the lengths are settled here and only
-- the elements go back round.
elementwise :: (Data b) => Path -> Text -> [b] -> [b] -> Maybe Text
elementwise path what before after
  | length before /= length after = Just (describe path what)
  | otherwise = firstOf (zipWith (differ path) before after)

-- | Does this documentation comment say anything?
saysNothing :: HsDocString -> Bool
saysNothing = null . docWords

-- | A documentation comment, taken as absent where it says nothing.
--
-- 'Nothing' where the two are not a @Maybe (LHsDoc GhcPs)@, which sends
-- 'differ' on to the next @as…@ function. Otherwise the answer, itself
-- 'Nothing' where the two say the same thing.
asStandaloneDoc :: (Data a) => Path -> a -> a -> Maybe (Maybe Text)
asStandaloneDoc path x y = case (cast x, cast y) of
  (Just before, Just after) -> Just (compared (kept before) (kept after))
  _ -> Nothing
  where
    kept :: Maybe (LHsDoc GhcPs) -> Maybe (LHsDoc GhcPs)
    kept d = if any (saysNothing . hsDocString . unLoc) d then Nothing else d
    compared before after = case (before, after) of
      (Nothing, Nothing) -> Nothing
      (Just b, Just a) -> differ (toConstr before : path) b a
      _ ->
        Just
          ( describe
              path
              (named (toConstr before) <> " became " <> named (toConstr after))
          )

-- | An export list, minus the documentation that says nothing.
asExportItems :: (Data a) => Path -> a -> a -> Maybe (Maybe Text)
asExportItems path x y = case (cast x, cast y) of
  (Just before, Just after) ->
    Just (elementwise path "the module exports a different list" (kept before) (kept after))
  _ -> Nothing
  where
    kept :: [LIE GhcPs] -> [LIE GhcPs]
    kept = filter (not . emptyExport . unLoc)
    emptyExport = \case
      IEDoc _ doc -> saysNothing (hsDocString (unLoc doc))
      _ -> False

-- | A block of declarations, minus the documentation that says nothing.
asDeclarations :: (Data a) => Path -> a -> a -> Maybe (Maybe Text)
asDeclarations path x y = case (cast x, cast y) of
  (Just before, Just after) ->
    Just (elementwise path "a different number of declarations" (kept before) (kept after))
  _ -> Nothing
  where
    kept :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
    kept = filter (not . emptyDecl . unLoc)
    emptyDecl = \case
      DocD _ d -> case d of
        DocCommentNext doc -> saysNothing (hsDocString (unLoc doc))
        DocCommentPrev doc -> saysNothing (hsDocString (unLoc doc))
        _ -> False
      _ -> False

-- | Does this type record only how or where something was written?
incidental :: TypeRep -> Bool
incidental rep = case splitTyConApp rep of
  (con, args)
    | qualified con == "GHC.Types.SrcLoc.GenLocated" -> False
    | notation con -> True
    | structural con -> not (null args) && all incidental args
    | otherwise -> False
  where
    qualified con = tyConModule con <> "." <> tyConName con

-- | Types that exist to record punctuation, position or spelling.
notation :: TyCon -> Bool
notation con =
  tyConModule con == "GHC.Parser.Annotation"
    || tyConModule con == "GHC.Types.SrcLoc"
    || ("GHC.Hs." `isPrefix` tyConModule con && "Ann" `isPrefix` tyConName con)
    || qualified `Set.member` alsoNotation
  where
    qualified = tyConModule con <> "." <> tyConName con
    isPrefix p t = take (length p) t == p

-- | The stragglers, named in full.
alsoNotation :: Set String
alsoNotation =
  Set.fromList
    [ -- Where a layout block's column was, which is the whole of what
      -- reformatting changes.
      "GHC.Hs.Extension.EpLayout",
      "Language.Haskell.Syntax.Extension.EpLayout",
      -- The text GHC keeps beside a literal or a pragma so that it can
      -- reproduce what was typed: the spaces inside @{-# INLINE   f #-}@,
      -- whether an integer was written in hex, how a multi-line string was
      -- indented. The value itself is in the next field along.
      "GHC.Types.SourceText.SourceText",
      -- Whether a linear arrow was written @%1 ->@ or @⊸@. The multiplicity
      -- it stands for is a different field, and is compared.
      "GHC.Hs.Type.EpLinear"
    ]

-- | Containers that are transparent to the question.
structural :: TyCon -> Bool
structural con =
  tyConName con
    `elem` ["Maybe", "List", "NonEmpty", "Tuple2", "Tuple3", "Tuple4", "Tuple5"]

-- | Which side of the module name @qualified@ was written on.
asQualifiedStyle :: (Data a) => Path -> a -> a -> Maybe (Maybe Text)
asQualifiedStyle path x y = case (cast x, cast y) of
  (Just before, Just after) -> Just (compared before after)
  _ -> Nothing
  where
    compared :: ImportDeclQualifiedStyle -> ImportDeclQualifiedStyle -> Maybe Text
    compared before after
      | isImportDeclQualified before == isImportDeclQualified after = Nothing
      | otherwise = Just (describe path "the import stopped being qualified")

-- | A @deriving@ clause, however it was punctuated.
asDerivingClause :: (Data a) => Path -> a -> a -> Maybe (Maybe Text)
asDerivingClause path x y = case (cast x, cast y) of
  (Just before, Just after) ->
    Just (differ path (derived before) (derived after))
  _ -> Nothing
  where
    derived :: DerivClauseTys GhcPs -> [LHsSigType GhcPs]
    derived = \case
      DctSingle _ t -> [t]
      DctMulti _ ts -> ts

-- | A module's imports, compared as the set they are.
asImports :: (Data a) => Path -> a -> a -> Maybe (Maybe Text)
asImports path x y = case (cast x, cast y) of
  (Just before, Just after) -> Just (alongside (normalised before) (normalised after))
  _ -> Nothing
  where
    normalised :: [LImportDecl GhcPs] -> [LImportDecl GhcPs]
    normalised = normalizeImports (Is #implicitPrelude) [] []
    alongside before after
      | length before /= length after =
          Just (describe path "the module imports a different set of modules")
      | otherwise = firstOf (zipWith (differ path) before after)

-- | A context, compared for the constraints it holds.
asContext :: (Data a) => Path -> a -> a -> Maybe (Maybe Text)
asContext path x y = compared optional <|> compared written <|> compared quoted
  where
    compared :: forall b c. (Typeable b, Data c) => (b -> c) -> Maybe (Maybe Text)
    compared strip = case (cast x, cast y) of
      (Just before, Just after) -> Just (differ path (strip before) (strip after))
      _ -> Nothing
    optional :: Maybe (LHsContext GhcPs) -> [HsType GhcPs]
    optional = maybe [] written
    written :: LHsContext GhcPs -> [HsType GhcPs]
    written = fmap (unbracket . unLoc) . unLoc
    quoted :: XRec GhcPs [LHsExpr GhcPs] -> [HsExpr GhcPs]
    quoted = fmap (unparenthesised . unLoc) . unLoc
    unbracket = \case
      HsParTy _ t -> unbracket (unLoc t)
      t -> t
    unparenthesised = \case
      HsPar _ e -> unparenthesised (unLoc e)
      e -> e

-- | A Haddock, compared for what it documents.
asDocString :: (Data a) => Path -> a -> a -> Maybe (Maybe Text)
asDocString path x y = case (cast x, cast y) of
  (Just before, Just after)
    | summarised before == summarised after -> Just Nothing
    | otherwise ->
        Just (Just (describe path "a documentation comment changed"))
  _ -> Nothing
  where
    summarised :: HsDocString -> (Text, [ByteString])
    summarised d = (kindOf d, docWords d)
    kindOf = \case
      MultiLineDocString dec _ -> decorator dec
      NestedDocString dec _ -> decorator dec
      GeneratedDocString _ -> "generated"
    decorator = \case
      HsDocStringNext -> "pointer"
      HsDocStringPrevious -> "pointer"
      HsDocStringNamed n -> "named " <> T.pack n
      HsDocStringGroup n -> "group " <> T.pack (show n)

-- | What a doc string says, with the whitespace thrown away.
docWords :: HsDocString -> [ByteString]
docWords =
  concatMap chunkWords . \case
    MultiLineDocString _ cs -> fmap unLoc (NE.toList cs)
    NestedDocString _ c -> [unLoc c]
    GeneratedDocString c -> [c]
  where
    chunkWords (HsDocStringChunk bytes) =
      filter (not . BS.null) (BS.splitWith isAsciiSpace bytes)
    isAsciiSpace w = w == 32 || w == 9 || w == 10 || w == 13

-- | Compare two values of a type that will not be taken apart.
opaque :: forall a b. (Data a, Data b) => a -> b -> Bool
opaque x y = case dataTypeName (dataTypeOf x) of
  "FastString" -> by @FastString
  "OccName" -> by @OccName
  "ModuleName" -> by @ModuleName
  "Name" -> by @Name
  "Unit" -> by @Unit
  "Data.ByteString.ByteString" -> by @ByteString
  name ->
    error $
      "Tilia.Equivalence: "
        <> name
        <> " does not expose its structure and is not one of the types this\
           \ knows how to compare. Decide whether it carries meaning and add\
           \ it to `opaque`, or to `alsoOnlyAboutPlacement` if it is a\
           \ position."
  where
    by :: forall t. (Typeable t, Eq t) => Bool
    by = case (cast x, cast y) of
      (Just p, Just q) -> p == (q :: t)
      _ -> False

typeNameOf :: (Data a) => a -> String
typeNameOf = dataTypeName . dataTypeOf

----------------------------------------------------------------------------
-- Comments

-- | Did every comment survive, and if not, which one and how?
--
-- Ordinary comments are compared as they will be printed, in order: the
-- text is normalised on the way in, so a comment that came out unchanged
-- reads back identically, and one that was mangled or moved past its
-- neighbor does not.
--
-- Except in the header, where the order says nothing. The pragmas are
-- sorted and so are the imports, and a comment written against either
-- travels with it, so the two streams are put in a settled order there
-- before being compared. What is still asked of that stretch is that the
-- same comments come out of it.
--
-- Documentation comments are counted rather than compared. The printer may
-- legitimately rewrite one—a @-- ^ x@ that moves in front of what it
-- documents has to become @-- | x@—so the text is not expected to survive,
-- but the comment is.
commentDifference ::
  -- | The module each stream came from, which is asked only how far down its
  -- header reaches.
  (HsModule GhcPs, HsModule GhcPs) ->
  [Comment] ->
  [Comment] ->
  Maybe Text
commentDifference (moduleBefore, moduleAfter) before0 after0
  | not (Set.null lost) = Just ("lost the pragma " <> pragmaList lost)
  | not (Set.null gained) = Just ("invented the pragma " <> pragmaList gained)
  | docsBefore /= docsAfter =
      Just $
        "the module's "
          <> tshow docsBefore
          <> " documentation comments became "
          <> tshow docsAfter
  | otherwise =
      diverge (belowHeader moduleBefore before) (belowHeader moduleAfter after)
        <|> diverge
          (settled (withinHeader moduleBefore before))
          (settled (withinHeader moduleAfter after))
  where
    before = escapedAndSplit before0
    after = escapedAndSplit after0

    belowHeader m = filter (not . inHeader m) . ordinary
    withinHeader m = filter (inHeader m) . ordinary
    settled = sortOn bodyKey

    inHeader m c = case lastRearrangedLine m of
      Nothing -> False
      Just lastLine -> spanStartLine (commentSpan c) <= lastLine

    escapedAndSplit = concatMap explode

    explode c = case commentStyle c of
      DocComment
        | "--" `T.isPrefixOf` NE.head (commentBody c) ->
            [ c{commentBody = l :| [], commentCodeBeforeStopsAt = before'}
            | (n, l) <- zip [0 :: Int ..] (NE.toList (body (escapeTrigger c))),
              let before' =
                    if n == 0 then commentCodeBeforeStopsAt c else Nothing
            ]
        | otherwise -> [escapeTrigger c]
      _ -> [c]
    body = commentBody

    lost = pragmasOf before `Set.difference` pragmasOf after
    gained = pragmasOf after `Set.difference` pragmasOf before
    docsBefore = length (documentation before)
    docsAfter = length (documentation after)

    documentation = filter isDocumentation
    isDocumentation = triggerEscaped

    ordinary =
      filter (\c -> not (isDocumentation c) && isNothing (commentPragma c))

    diverge [] [] = Nothing
    diverge (b : _) [] = Just ("lost " <> quoted b)
    diverge [] (a : _) = Just ("gained " <> quoted a)
    diverge (b : bs) (a : as)
      | bodyKey b == bodyKey a = diverge bs as
      | Just (bs', as') <- crossed (b : bs) (a : as) = diverge bs' as'
      | otherwise = Just (quoted b <> " became " <> quoted a)

    crossed written printed =
      listToMaybe
        [ (drop (j + k) written, drop (j + k) printed)
        | j <- [1 .. length (takeWhile commentTrailing written)],
          let lifted = drop j written,
          let shared = length (takeWhile id (zipWith alike printed lifted)),
          k <- [shared, shared - 1 .. 1],
          all (not . commentTrailing) (take k lifted),
          fmap bodyKey (take j (drop k printed)) == fmap bodyKey (take j written)
        ]

    alike x y = bodyKey x == bodyKey y

    quoted c = "`" <> T.intercalate "\\n" (NE.toList (commentBody c)) <> "`"
    pragmaList =
      T.intercalate ", "
        . fmap (\(n, b) -> "{-# " <> n <> " " <> b <> " #-}")
        . Set.toList

-- | A comment's lines, as they are compared.
bodyKey :: Comment -> NonEmpty Text
bodyKey c = case commentBody c of
  (l :| []) -> T.stripStart l :| []
  ls -> ls

-- | The last line the formatter may reorder: the end of the last import, or
-- of the module header where there are no imports.
--
-- 'Nothing' where the module has neither header nor imports.
lastRearrangedLine :: HsModule GhcPs -> Maybe Int
lastRearrangedLine m = spanEndLine <$> (spansOf (hsmodImports m) <> header)
  where
    header =
      foldMap spanOf (hsmodName m)
        <> foldMap spanOf (hsmodDeprecMessage (hsmodExt m))
        <> foldMap spanOf (hsmodExports m)

-- | The pragmas a module carries, however they were written.
--
-- A @{-# LANGUAGE A, B #-}@ counts as two, because that is what the
-- formatter turns it into, and the name is upper-cased and the body trimmed
-- so that two spellings of the same pragma are the same pragma.
pragmasOf :: [Comment] -> Set (Text, Text)
pragmasOf = Set.fromList . concatMap entries . mapMaybe commentPragma
  where
    entries p
      | pragmaName p == "LANGUAGE" =
          [ ("LANGUAGE", extension)
          | e <- T.splitOn "," (pragmaBody p),
            let extension = T.strip e,
            not (T.null extension)
          ]
      | otherwise = [(pragmaName p, pragmaBody p)]
