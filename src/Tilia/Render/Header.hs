{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The module header, and the module as a whole.
module Tilia.Render.Header
  ( HeaderPragma (..),
    takeHeaderPragmas,
    takeStackHeader,
    hsModule,
  )
where

import Data.Function (on)
import Data.List (sortOn)
import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Driver.Flags (Language)
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.PkgQual (RawPkgQual (..))
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Comments (Comment (..), Pragma (..), commentPragma)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Declaration (decls)
import Tilia.Render.Haddock
import Tilia.Render.Layout
import Tilia.Render.Name
import Tilia.Render.Pragma (warningTxt)
import Tilia.Source (Source, directivePresentOnLine, sourceLines)
import Tilia.Span
import Tilia.Span.Ghc

-- | A pragma of the file header.
data HeaderPragma = HeaderPragma
  { -- | The region it was written in.
    hpSpan :: Span,
    -- | How many preprocessor directives the header has above it.
    hpRun :: Int,
    -- | Where it sorts.
    hpOrder :: PragmaOrder,
    -- | @LANGUAGE@, @OPTIONS_GHC@ or @OPTIONS_HADDOCK@.
    hpName :: Text,
    -- | One extension, or the whole of an options string.
    hpBody :: Text
  }
  deriving (Eq, Show)

-- | Where a pragma sorts among the others.
--
-- The derived ordering is the whole of the policy: language pragmas first,
-- then @OPTIONS_GHC@, then @OPTIONS_HADDOCK@; and within the language
-- pragmas, by the class of extension.
data PragmaOrder
  = LanguageOrder ExtensionClass
  | OptionsGhcOrder
  | OptionsHaddockOrder
  deriving (Eq, Ord, Show)

-- | Which group an extension sorts into.
--
-- Sorting the extensions alphabetically outright would change what a module
-- means, because an extension can turn others on and a later one can turn
-- them off again. Sorting only within these groups keeps the relationships
-- that matter: a pack before what it enables, an enabling before a
-- disabling, and the stragglers that have to come last at the end.
data ExtensionClass
  = -- | @GHC2021@, @Haskell2010@ and the like.
    Pack
  | -- | Anything else.
    Enabling
  | -- | An extension written with a @No@ prefix.
    Disabling
  | -- | Extensions that only work when nothing follows them.
    Last'
  deriving (Eq, Ord, Show)

-- | Pick the header pragmas out of a comment stream.
takeHeaderPragmas ::
  -- | The module as written.
  Source ->
  -- | Where the header ends.
  Maybe Span ->
  -- | The comment stream.
  [Comment] ->
  ([HeaderPragma], [Comment])
takeHeaderPragmas src headerEnd comments = (pragmas, plain)
  where
    recognised = [(c, headerPragma c) | c <- comments]
    pragmas = [entry c p | (c, Just p) <- recognised]
    plain =
      [ if rightAbovePragma c then airless c else c
      | (c, Nothing) <- recognised
      ]
    rightAbovePragma c =
      Set.member (below (spanEndLine (commentSpan c) + 1)) pragmaStarts
    below n = if directivePresentOnLine n (sourceLines src) then below (n + 1) else n
    pragmaStarts =
      Set.fromList [spanStartLine (commentSpan c) | (c, Just _) <- recognised]
    airless c = c{commentGapAbove = False, commentGapBelow = False}
    directivesAbove n =
      length [k | k <- [1 .. n - 1], directivePresentOnLine k (sourceLines src)]
    entry c p =
      HeaderPragma
        { hpSpan = commentSpan c,
          hpRun = directivesAbove (spanStartLine (commentSpan c)),
          hpOrder = orderOf p,
          hpName = pragmaName p,
          hpBody = pragmaBody p
        }
    headerPragma c = do
      p <- commentPragma c
      _ <- lookupOrder (pragmaName p)
      if inHeader headerEnd (commentSpan c) then Just p else Nothing
    orderOf p = case pragmaName p of
      "LANGUAGE" -> LanguageOrder (classifyExtension (pragmaBody p))
      other -> maybe OptionsGhcOrder id (lookupOrder other)
    lookupOrder = \case
      "LANGUAGE" -> Just (LanguageOrder Enabling)
      "OPTIONS_GHC" -> Just OptionsGhcOrder
      "OPTIONS_HADDOCK" -> Just OptionsHaddockOrder
      _ -> Nothing

-- | Was this written above everything the compiler reads as code?
inHeader :: Maybe Span -> Span -> Bool
inHeader headerEnd s = case headerEnd of
  Nothing -> True
  Just end -> spanStartLine s < spanStartLine end

-- | Take the Stack script header off the front of a comment stream.
takeStackHeader ::
  -- | Where the header ends.
  Maybe Span ->
  -- | The comment stream.
  [Comment] ->
  (Doc, [Comment])
takeStackHeader headerEnd = \case
  (c : cs) | isStackHeader c -> (reproduce c <> blankLine, cs)
  cs -> (mempty, cs)
  where
    isStackHeader c =
      inHeader headerEnd (commentSpan c)
        && T.isPrefixOf "stack" (T.stripStart (T.drop 2 (NE.head (commentBody c))))
    reproduce c =
      sepBy (verbatimBreak AtMargin TrimWhitespace) (fmap txt (NE.toList (commentBody c)))

-- | The pragmas of a header, one per line, sorted.
pragmaBlock :: [HeaderPragma] -> Doc
pragmaBlock = foldMap render . dedupe . sortOn key . concatMap split
  where
    key p = (hpRun p, hpOrder p, hpBody p)
    dedupe = fmap NE.head . NE.groupBy ((==) `on` key)
    split p
      | hpName p == "LANGUAGE" =
          [ p{hpBody = body, hpOrder = LanguageOrder (classifyExtension body)}
          | body <- fmap T.strip (T.splitOn "," (hpBody p))
          ]
      | otherwise = [p]

    render p =
      located
        (hpSpan p)
        (txt "{-# " <> txt (hpName p) <> space <> txt (hpBody p) <> txt " #-}")
        <> hardBreak

-- | Which group an extension belongs to.
classifyExtension :: Text -> ExtensionClass
classifyExtension t
  | namesAnEdition t = Pack
  -- @ImplicitPrelude@ and @CUSKs@ are turned off by other extensions, so
  -- asking for either of them only takes effect at the end.
  | t == "ImplicitPrelude" = Last'
  | t == "CUSKs" = Last'
  | otherwise = case T.uncons (T.drop 2 t) of
      Just (c, _) | "No" `T.isPrefixOf` t, c `elem` ['A' .. 'Z'] -> Disabling
      _ -> Enabling

-- | Does this name a whole edition of the language rather than one
-- extension of it?
namesAnEdition :: Text -> Bool
namesAnEdition t = any spelledTheSame [minBound .. maxBound]
  where
    spelledTheSame edition = t == T.pack (show (edition :: Language))

-- | A whole module.
hsModule :: Ctx -> [HeaderPragma] -> HsModule GhcPs -> Doc
hsModule ctx pragmas HsModule{hsmodExt = XModulePs{..}, ..} =
  headerLayout $
    pragmaBlock pragmas
      <> hardBreak
      <> moduleLine
      <> hardBreak
      <> foldMap (\i -> at_ ctx (importDecl ctx) i <> hardBreak) hsmodImports
      <> hardBreak
      <> layoutFrom ctx (spansOf hsmodDecls) (decls ctx Free hsmodDecls)
  where
    exports = maybe [] unLoc hsmodExports
    headerSpan = foldMap spanOf hsmodDeprecMessage <> foldMap spanOf hsmodExports
    headerLayout
      | any (isDocEntry . unLoc) exports = broken
      | otherwise = layoutFrom ctx headerSpan
    moduleLine = case hsmodName of
      Nothing -> mempty
      Just modName ->
        documentation
          <> at ctx modName (moduleHeadName ctx)
          <> breakOrSpace
          <> foldMap (\w -> at ctx w warningTxt <> breakOrSpace) hsmodDeprecMessage
          <> foldMap exports' hsmodExports
          <> txt "where"
          <> hardBreak
    documentation = foldMap (haddock ctx Pipe Closed) hsmodHaddockModHeader
    exports' l =
      at ctx l (\xs -> indent (exportList ctx (spanOf l) xs)) <> breakOrSpace

-- | The parenthesised list after a module name.
exportList :: Ctx -> Maybe Span -> [LIE GhcPs] -> Doc
exportList ctx enclosing xs =
  layoutHere . parens . insideBrackets enclosing $
    importExportItems ctx xs
  where
    layoutHere
      | any (isDocEntry . unLoc) xs = broken
      | otherwise = layoutFrom ctx enclosing

-- | The items of an import or export list.
importExportItems :: Ctx -> [LIE GhcPs] -> Doc
importExportItems ctx xs = variant (laidOut False) (laidOut True)
  where
    laidOut broken' =
      sepBy breakOrSpace (zipWith (item broken') (Nothing : fmap Just xs) (places xs))
    item broken' previous (place, x) =
      gapAbove place (unLoc <$> previous) (unLoc x)
        <> align (at ctx (widenToDoc x) (ieItem ctx (spanOf x) (comma' broken' place)))
    gapAbove place previous here
      | place == First || place == Only = mempty
      | isSection here = hardBreak
      | isPipe here, maybe False runsOn previous = hardBreak
      | otherwise = mempty
    isSection = \case
      IEGroup{} -> True
      _ -> False
    isPipe = \case
      IEDoc{} -> True
      _ -> False
    runsOn = \case
      IEDoc{} -> True
      IEDocNamed{} -> True
      _ -> False
    comma' broken' place
      | broken' = True
      | otherwise = place == First || place == Middle

-- | Widen an item's span to take in the documentation printed with it, so
-- that a documented item is laid out as one thing.
widenToDoc :: LIE GhcPs -> LIE GhcPs
widenToDoc l@(L ann ie) = case itemDoc ie of
  Nothing -> l
  Just (L docSpan _) -> L (ann <> noAnnSrcSpan docSpan) ie

-- | One item of an import or export list.
ieItem :: Ctx -> Maybe Span -> Bool -> IE GhcPs -> Doc
ieItem ctx here withComma = \case
  IEVar warning n doc ->
    exportWarning warning
      <> at ctx n (wrappedName ctx)
      <> comma'
      <> itemDocumentation doc
  IEThingAbs warning n doc ->
    exportWarning warning
      <> at ctx n (wrappedName ctx)
      <> comma'
      <> itemDocumentation doc
  IEThingAll (warning, _) n doc ->
    exportWarning warning
      <> at ctx n (wrappedName ctx)
      <> space
      <> txt "(..)"
      <> comma'
      <> itemDocumentation doc
  IEThingWith (warning, _) n wildcard members doc ->
    align
      ( exportWarning warning
          <> at ctx n (wrappedName ctx)
          <> breakOrSpace
          <> indent (parens (insideBrackets here (commaSep (align <$> withWildcard))))
          <> comma'
      )
      <> itemDocumentation doc
    where
      rendered = fmap (at_ ctx (wrappedName ctx)) members
      withWildcard = case wildcard of
        NoIEWildcard -> rendered
        IEWildcard n' ->
          let (before, after) = splitAt n' rendered
           in before <> [txt ".."] <> after
  IEModuleContents (warning, _) m ->
    exportWarning warning <> at ctx m (moduleHeadName ctx) <> comma'
  IEGroup NoExtField n str -> haddock ctx (Section n) Open str
  IEDoc NoExtField str -> haddock ctx Pipe Open str
  IEDocNamed NoExtField n -> case writtenHaddock ctx here of
    Just written -> sepBy (verbatimBreak AtIndent TrimWhitespace) (fmap txt (NE.toList written))
    Nothing -> txt (docSectionName n)
  where
    comma' = includeWhen withComma comma
    exportWarning =
      foldMap (\w -> at ctx w warningTxt <> breakOrSpace)
    itemDocumentation =
      foldMap (\d -> breakOrSpace <> haddock ctx Caret Open d)

-- | The documentation written with an export list item, if any.
itemDoc :: IE GhcPs -> Maybe (ExportDoc GhcPs)
itemDoc = \case
  IEVar _ _ doc -> doc
  IEThingAbs _ _ doc -> doc
  IEThingAll _ _ doc -> doc
  IEThingWith _ _ _ _ doc -> doc
  _ -> Nothing

-- | Does this export list entry carry documentation?
isDocEntry :: IE GhcPs -> Bool
isDocEntry = \case
  IEDoc{} -> True
  IEGroup{} -> True
  IEDocNamed{} -> True
  _ -> False

-- | One import declaration.
importDecl :: Ctx -> ImportDecl GhcPs -> Doc
importDecl ctx ImportDecl{..} =
  txt "import"
    <> space
    <> includeWhen (ideclSource == IsBoot) (txt "{-# SOURCE #-}")
    <> space
    <> includeWhen ideclSafe (txt "safe")
    <> space
    <> levelBefore
    <> space
    <> includeWhen (isQualified && not qualifiedLast) (txt "qualified")
    <> space
    <> packageQualifier
    <> space
    <> indent
      ( at ctx ideclName outputable
          <> space
          <> levelAfter
          <> includeWhen (isQualified && qualifiedLast) (space <> txt "qualified")
          <> foldMap (\a -> space <> txt "as" <> space <> at ctx a outputable) ideclAs
          <> space
          <> importList
      )
  where
    qualifiedLast = extensionOn ctx ImportQualifiedPost
    isQualified = isImportDeclQualified ideclQualified
    packageQualifier = case ideclPkgQual of
      NoRawPkgQual -> mempty
      RawPkgQual literal -> outputable literal
    levelBefore = case ideclLevelSpec of
      LevelStylePre l -> declLevel l
      _ -> mempty
    levelAfter = case ideclLevelSpec of
      LevelStylePost l -> declLevel l
      _ -> mempty
    importList = case ideclImportList of
      Nothing -> mempty
      Just (interpretation, L listLoc xs) ->
        hidden
          <> breakOrSpace
          <> parens
            ( insideBrackets
                (spanOfSrcSpan (locA listLoc))
                (importExportItems ctx xs)
            )
        where
          hidden = case interpretation of
            Exactly -> mempty
            EverythingBut -> txt "hiding"

-- | The keyword an import's level is written with.
declLevel :: ImportDeclLevel -> Doc
declLevel = \case
  ImportDeclSplice -> txt "splice"
  ImportDeclQuote -> txt "quote"
