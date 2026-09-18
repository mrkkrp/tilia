{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Declarations: dispatching to the right printer, and grouping.
--
-- Two jobs live here. The first is a case over every kind of declaration,
-- which is mostly a matter of handing the work on; the few forms with no
-- module of their own—foreign imports, annotations, @default@ declarations,
-- top-level splices—are printed here rather than in four files of a dozen
-- lines each.
--
-- The second is grouping, which is the interesting one. A blank line
-- between declarations is meaningful to a reader, so a signature and the
-- function it describes should stay together while unrelated declarations
-- are kept apart. Nothing in the syntax tree says which declarations belong
-- together, so it is worked out from what they are and what they name.
module Tilia.Render.Declaration
  ( decls,
    declsKeepingGroups,
  )
where

import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import GHC.Data.FastString (unpackFS)
import GHC.Hs
import GHC.Types.ForeignCall (CExportSpec (..))
import GHC.Types.Name.Occurrence (occNameFS)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), isGoodSrcSpan, unLoc)
import Tilia.Doc.Combinators
import Tilia.Render.Class
import Tilia.Render.Context
import Tilia.Render.Data
import Tilia.Render.Expression
import Tilia.Render.Haddock
import Tilia.Render.Layout
import Tilia.Render.Literal (stringLiteral)
import Tilia.Render.Name
import Tilia.Render.Pragma
import Tilia.Render.Signature
import Tilia.Render.Type
import Tilia.Source (SourceType (..))
import Tilia.Span
import Tilia.Span.Ghc

-- | A run of declarations, with blank lines wherever we think they belong.
decls :: Ctx -> FamilyStyle -> [LHsDecl GhcPs] -> Doc
decls = declRun Disregard

-- | A run of declarations that keeps the author's grouping.
declsKeepingGroups :: Ctx -> FamilyStyle -> [LHsDecl GhcPs] -> Doc
declsKeepingGroups = declRun Respect

-- | Whether the author's own blank lines are consulted.
data Grouping
  = Disregard
  | Respect
  deriving (Eq, Show)

declRun :: Grouping -> Ctx -> FamilyStyle -> [LHsDecl GhcPs] -> Doc
declRun grouping ctx style ds =
  items NoBrace $ case groups of
    [] -> []
    (firstGroup : rest) ->
      render firstGroup <> concat (zipWith withGap groups rest)
  where
    isSignatureFile = ctxSourceType ctx == SignatureSource
    groups = groupDecls ctx isSignatureFile ds
    render = NE.toList . fmap (at_ ctx (hsDecl ctx style))

    withGap previous current
      | separate previous current = breakOrSpace : render current
      | otherwise = render current

    separate previous current = case grouping of
      Disregard -> True
      Respect ->
        separatedByBlank ctx ended began
          || commentBetween ctx ended began
          || isDocumented previous
          || isDocumented current
      where
        ended = spanOf (NE.last previous)
        began = spanOf (NE.head current)

    isDocumented = any (isDocNext . unLoc)
    isDocNext = \case
      DocD _ (DocCommentNext _) -> True
      DocD _ (DocCommentPrev _) -> True
      _ -> False

-- | Gather declarations that belong together.
groupDecls :: Ctx -> Bool -> [LHsDecl GhcPs] -> [NonEmpty (LHsDecl GhcPs)]
groupDecls _ _ [] = []
groupDecls ctx isSignatureFile (d : ds)
  | isDocNext (unLoc d) = case groupDecls ctx isSignatureFile ds of
      [] -> [d :| []]
      (g : gs)
        | isDoc (unLoc (NE.head g)) -> (d :| []) : g : gs
        | otherwise -> (d <| g) : gs
  | otherwise =
      let (together, rest) = span belongs (zip (d : ds) ds)
       in (d :| map snd together) : groupDecls ctx isSignatureFile (map snd rest)
  where
    isDocNext = \case
      DocD _ (DocCommentNext _) -> True
      _ -> False
    isDoc = \case
      DocD _ _ -> True
      _ -> False
    belongs (previous, current) =
      (not isSignatureFile && isSignatureSeries ctx previous current)
        || isDerivingSeries ctx previous current
        || relatedDecls d current
        || relatedDecls previous current

-- | A run of type signatures with nothing between them is a list, and a
-- list reads better without gaps in it.
isSignatureSeries :: Ctx -> LHsDecl GhcPs -> LHsDecl GhcPs -> Bool
isSignatureSeries ctx x@(L _ a) y@(L _ b) = case (a, b) of
  (SigD _ TypeSig {}, SigD _ TypeSig {}) ->
    not (commentBetween ctx (spanOf x) (spanOf y))
  _ -> False

-- | Two standalone @deriving@ declarations the author ran together.
isDerivingSeries :: Ctx -> LHsDecl GhcPs -> LHsDecl GhcPs -> Bool
isDerivingSeries ctx x@(L _ a) y@(L _ b) = case (a, b) of
  (DerivD {}, DerivD {}) ->
    not (separatedByBlank ctx (spanOf x) (spanOf y))
  _ -> False

-- | The kinds of declaration that grouping distinguishes.
data Kind
  = TypeSignature
  | DefaultSignature
  | FunctionBody
  | PatternSignature
  | PatternDefinition
  | DataDeclaration
  | ClassDeclaration
  | KindSignature
  | FamilyDeclaration
  | TypeSynonym
  | PragmaDeclaration
  | TopLevelSplice
  | DocumentsNext
  | DocumentsPrevious
  | Unremarkable
  deriving (Eq, Show)

-- | What a declaration is, and what it names.
declKind :: HsDecl GhcPs -> (Kind, [RdrName])
declKind = \case
  SigD _ (TypeSig _ ns _) -> (TypeSignature, map unLoc ns)
  SigD _ (ClassOpSig _ True ns _) -> (DefaultSignature, map unLoc ns)
  SigD _ (ClassOpSig _ False ns _) -> (TypeSignature, map unLoc ns)
  SigD _ (PatSynSig _ ns _) -> (PatternSignature, map unLoc ns)
  SigD _ (InlineSig _ (L _ n) _) -> (PragmaDeclaration, [n])
  SigD _ (SCCFunSig _ (L _ n) _) -> (PragmaDeclaration, [n])
  SigD _ sig
    | Just n <- specialisedName sig -> (PragmaDeclaration, [n])
  ValD _ (FunBind _ (L _ n) _) -> (FunctionBody, [n])
  ValD _ (PatBind _ p _ _) -> (FunctionBody, boundNames p)
  ValD _ (PatSynBind _ (PSB _ (L _ n) _ _ _)) -> (PatternDefinition, [n])
  AnnD _ (HsAnnotation _ (ValueAnnProvenance (L _ n)) _) -> (PragmaDeclaration, [n])
  AnnD _ (HsAnnotation _ (TypeAnnProvenance (L _ n)) _) -> (PragmaDeclaration, [n])
  WarningD _ (Warnings _ ws) ->
    (PragmaDeclaration, [unLoc n | L _ (Warning _ ns _) <- ws, n <- ns])
  TyClD _ (DataDecl _ (L _ n) _ _ _) -> (DataDeclaration, [n])
  TyClD _ (ClassDecl {tcdLName = L _ n}) -> (ClassDeclaration, [n])
  TyClD _ (SynDecl _ (L _ n) _ _ _) -> (TypeSynonym, [n])
  TyClD _ (FamDecl _ (FamilyDecl _ _ _ (L _ n) _ _ _ _)) -> (FamilyDeclaration, [n])
  KindSigD _ (StandaloneKindSig _ (L _ n) _) -> (KindSignature, [n])
  SpliceD _ (SpliceDecl _ _ _) -> (TopLevelSplice, [])
  DocD _ (DocCommentNext _) -> (DocumentsNext, [])
  DocD _ (DocCommentPrev _) -> (DocumentsPrevious, [])
  _ -> (Unremarkable, [])

-- | The names a pattern binding brings into scope.
boundNames :: LPat GhcPs -> [RdrName]
boundNames (L _ p) = case p of
  VarPat _ (L _ n) -> [n]
  AsPat _ (L _ n) inner -> n : boundNames inner
  NPlusKPat _ (L _ n) _ _ _ _ -> [n]
  LazyPat _ inner -> boundNames inner
  BangPat _ inner -> boundNames inner
  ParPat _ inner -> boundNames inner
  SigPat _ inner _ -> boundNames inner
  ViewPat _ _ inner -> boundNames inner
  SumPat _ inner _ _ -> boundNames inner
  TuplePat _ ps _ -> concatMap boundNames ps
  ListPat _ ps -> concatMap boundNames ps
  OrPat _ ps -> concatMap boundNames (NE.toList ps)
  ConPat _ _ details -> concatMap boundNames (hsConPatArgs details)
  _ -> []

-- | Should these two declarations be printed with no blank line between
-- them?
relatedDecls :: LHsDecl GhcPs -> LHsDecl GhcPs -> Bool
relatedDecls a b = case (kindA, kindB) of
  (DocumentsNext, _) -> True
  (_, DocumentsPrevious) -> True
  (TopLevelSplice, TopLevelSplice) -> not (blankBetween (spanOf a) (spanOf b))
  pair | pair `elem` relatedKinds -> shareAName namesA namesB
  _ -> False
  where
    (kindA, namesA) = declKind (unLoc a)
    (kindB, namesB) = declKind (unLoc b)

-- | The pairs of declaration kinds that group when they name something in
-- common.
relatedKinds :: [(Kind, Kind)]
relatedKinds =
  [ (TypeSignature, FunctionBody),
    (TypeSignature, DefaultSignature),
    (DefaultSignature, TypeSignature),
    (DefaultSignature, FunctionBody),
    (TypeSignature, PragmaDeclaration),
    (PragmaDeclaration, TypeSignature),
    (PragmaDeclaration, FunctionBody),
    (FunctionBody, PragmaDeclaration),
    (PragmaDeclaration, DataDeclaration),
    (DataDeclaration, PragmaDeclaration),
    (PragmaDeclaration, PragmaDeclaration),
    (PatternSignature, PatternDefinition),
    (KindSignature, DataDeclaration),
    (KindSignature, ClassDeclaration),
    (KindSignature, FamilyDeclaration),
    (KindSignature, TypeSynonym)
  ]

-- | Do the two declarations name anything in common?
shareAName :: [RdrName] -> [RdrName] -> Bool
shareAName xs ys = overlaps (sort (map spelling xs)) (sort (map spelling ys))
  where
    spelling :: RdrName -> String
    spelling = unpackFS . occNameFS . rdrNameOcc
    overlaps (a : as) (b : bs)
      | a < b = overlaps as (b : bs)
      | a > b = overlaps (a : as) bs
      | otherwise = True
    overlaps _ _ = False

-- | Print one declaration.
hsDecl :: Ctx -> FamilyStyle -> HsDecl GhcPs -> Doc
hsDecl ctx style = \case
  TyClD _ x -> tyClDecl ctx style x
  ValD _ x -> valDecl ctx NoBrace x
  SigD _ x -> sigDecl ctx x
  InstD _ x -> instDecl ctx style x
  DerivD _ x -> standaloneDerivDecl ctx x
  DefD _ x -> defaultDecl ctx x
  ForD _ x -> foreignDecl ctx x
  WarningD _ x -> warnDecls ctx x
  AnnD _ x -> annDecl ctx x
  RuleD _ x -> ruleDecls ctx x
  SpliceD _ (SpliceDecl NoExtField splice deco) ->
    at ctx splice (untypedSplice ctx deco)
  RoleAnnotD _ x -> roleAnnot ctx x
  KindSigD _ x -> standaloneKindSig ctx x
  DocD _ x -> case x of
    DocCommentNext str -> haddock ctx Pipe Open str
    DocCommentPrev str -> haddock ctx Caret Open str
    DocCommentNamed n str -> haddock ctx (Chunk n) Open str
    DocGroup n str -> haddock ctx (Section n) Open str

tyClDecl :: Ctx -> FamilyStyle -> TyClDecl GhcPs -> Doc
tyClDecl ctx style = \case
  FamDecl _ x -> famDecl ctx style x
  SynDecl {..} -> synDecl ctx tcdLName tcdFixity tcdTyVars tcdRhs
  DataDecl {..} ->
    dataDecl
      ctx
      Associated
      tcdLName
      (hsq_explicit tcdTyVars)
      spanOf
      (at_ ctx (tyVarBndr ctx))
      tcdFixity
      mempty
      tcdDataDefn
  ClassDecl {tcdCExt = (anns, _, _), ..} ->
    classDecl
      ctx
      anns
      tcdCtxt
      tcdLName
      tcdTyVars
      tcdFixity
      tcdFDs
      tcdSigs
      tcdMeths
      tcdATs
      tcdATDefs
      tcdDocs

instDecl :: Ctx -> FamilyStyle -> InstDecl GhcPs -> Doc
instDecl ctx style = \case
  ClsInstD _ x -> clsInstDecl ctx x
  TyFamInstD _ x -> tyFamInstDecl ctx style x
  DataFamInstD _ x -> dataFamInstDecl ctx style x

-- | A @default@ declaration.
defaultDecl :: Ctx -> DefaultDecl GhcPs -> Doc
defaultDecl ctx (DefaultDecl _ className types) =
  txt "default"
    <> foldMap (\c -> breakOrSpace <> name ctx c) className
    <> breakOrSpace
    <> indent (parens (commaSep (map (align . hsType ctx) types)))

-- | An @ANN@ pragma.
annDecl :: Ctx -> AnnDecl GhcPs -> Doc
annDecl ctx (HsAnnotation _ provenance e) =
  pragma "ANN" . indent $
    subject <> breakOrSpace <> hsExpr ctx e
  where
    subject = case provenance of
      ValueAnnProvenance n -> name ctx n
      TypeAnnProvenance n -> txt "type" <> space <> name ctx n
      ModuleAnnProvenance -> txt "module"

-- | A foreign import or export.
foreignDecl :: Ctx -> ForeignDecl GhcPs -> Doc
foreignDecl ctx = \case
  fd@ForeignImport {fd_fi} -> foreignImport ctx fd_fi <> foreignSig ctx fd
  fd@ForeignExport {fd_fe} -> foreignExport ctx fd_fe <> foreignSig ctx fd

-- | The name and type that end a foreign declaration.
foreignSig :: Ctx -> ForeignDecl GhcPs -> Doc
foreignSig ctx fd =
  breakOrSpace
    <> indent
      ( layoutFrom ctx (spanOf (fd_name fd) <> spanOf (fd_sig_ty fd)) $
          name ctx (fd_name fd) <> typeAscription ctx (fd_sig_ty fd)
      )

-- | The head of a foreign import.
foreignImport :: Ctx -> ForeignImport GhcPs -> Doc
foreignImport ctx (CImport src callConv safety _ _) =
  txt "foreign import"
    <> space
    <> at ctx callConv outputable
    <> includeWhen (isGoodSrcSpan (getLocA safety)) (space <> outputable safety)
    <> indent
      ( at ctx src $ \case
          NoSourceText -> mempty
          SourceText lit -> breakOrSpace <> stringLiteral lit
      )

foreignExport :: Ctx -> ForeignExport GhcPs -> Doc
foreignExport ctx (CExport src (L loc (CExportStatic _ _ callConv))) =
  txt "foreign export"
    <> space
    <> at ctx (L loc callConv) outputable
    <> space
    <> at ctx src sourceText
