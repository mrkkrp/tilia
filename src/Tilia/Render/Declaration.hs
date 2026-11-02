{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Declarations: dispatching to the right printer, and grouping.
module Tilia.Render.Declaration
  ( decls,
    topLevelDecls,
    declsKeepingGroups,
  )
where

import Data.Choice (pattern Don't)
import Data.List (sort)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text qualified as T
import GHC.Data.FastString (unpackFS)
import GHC.Hs
import GHC.Types.ForeignCall (CExportSpec (..))
import GHC.Types.Name.Occurrence (occNameFS)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), isGoodSrcSpan, unLoc)
import Tilia.Doc.Combinators
import Tilia.Doc.Internal (printsOnOneLine)
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
import Tilia.Source (SourceType (..), blankAt, lineAt, sourceLines)
import Tilia.Span
import Tilia.Span.Ghc

-- | A run of declarations standing on their own, which keeps together the
-- one-line declarations of one kind the author wrote together.
decls ::
  -- | The context.
  Ctx ->
  -- | The declarations.
  [LHsDecl GhcPs] ->
  Doc
decls ctx = declRun apart ctx Free
  where
    apart previous current =
      kindOf previous /= kindOf current
        || any standsApart previous
        || any standsApart current
        || directiveBetween
          ctx
          (spanOf (fst (NE.last previous)))
          (spanOf (fst (NE.head current)))
    standsApart (d, printed) =
      isDoc (unLoc d) || not (printsOnOneLine Broken printed)
    kindOf group =
      listToMaybe
        [ k
        | (d, _) <- reverse (NE.toList group),
          let k = fst (declKind (unLoc d)),
          k /= PragmaDeclaration
        ]

-- | The top-level declarations of a module, with the lines a
-- @{- TILIA_OFF -}@ leaves as written printed as written.
topLevelDecls ::
  -- | The context.
  Ctx ->
  -- | The declarations.
  [LHsDecl GhcPs] ->
  Doc
topLevelDecls ctx ds = case ctxMagicCommentRanges ctx of
  [] -> decls ctx ds
  kept -> sepBy hardBreak (zipWith joined (Nothing : fmap Just ps) ps)
    where
      ps = pieces kept ds
  where
    ls = sourceLines (ctxSource ctx)
    joined previous piece = includeWhen gap hardBreak <> printed piece
      where
        gap = any (`blankAt` ls) (gapAbove piece <> foldMap gapBelow previous)
    gapAbove = \case
      Preserved from _ -> [from - 1]
      Formatted _ -> []
    gapBelow = \case
      Preserved _ to -> [to + 1]
      Formatted _ -> []
    printed = \case
      Formatted run -> decls ctx (NE.toList run)
      Preserved from to ->
        located
          (mkSpan (from, 1) (to, maybe 1 ((+ 1) . T.length) (lineAt to ls)))
          ( verbatimLines AtMargin KeepWhitespace $
              mapMaybe (`lineAt` ls) [from .. to]
          )

-- | A part of the top-level declarations: those formatted together, or the
-- lines a @{- TILIA_OFF -}@ leaves as written.
data Piece
  = -- | Declarations formatted together.
    Formatted (NonEmpty (LHsDecl GhcPs))
  | -- | The first and last line of a stretch left as written.
    Preserved Int Int

-- | Cut the top-level declarations at the lines left as written.
pieces ::
  -- | The first and last line of each stretch left as written.
  [(Int, Int)] ->
  -- | The declarations.
  [LHsDecl GhcPs] ->
  [Piece]
pieces kept ds = case kept of
  [] -> run ds
  (from, to) : rest ->
    let (before, after) = span (startsBefore from) ds
     in run before
          <> [Preserved from to]
          <> pieces rest (dropWhile (startsBefore (to + 1)) after)
  where
    run = foldMap (pure . Formatted) . NE.nonEmpty
    startsBefore n d = maybe False ((< n) . spanStartLine) (spanOf d)

-- | A run of declarations in a class or an instance, which keeps the
-- author's grouping.
declsKeepingGroups ::
  -- | The context.
  Ctx ->
  -- | The declarations.
  [LHsDecl GhcPs] ->
  Doc
declsKeepingGroups ctx = declRun apart ctx Associated
  where
    apart previous current = any documented previous || any documented current
    documented (d, _) = case unLoc d of
      DocD _ (DocCommentNext _) -> True
      DocD _ (DocCommentPrev _) -> True
      _ -> False

-- | Render a run of declarations, with an empty line wherever the author
-- left one and between groups kept apart.
declRun ::
  -- | Do these neighbouring groups, printed like this, get an empty line
  -- between them even where the author left none?
  ( NonEmpty (LHsDecl GhcPs, Doc) ->
    NonEmpty (LHsDecl GhcPs, Doc) ->
    Bool
  ) ->
  -- | The context.
  Ctx ->
  -- | How to print the families among the declarations.
  FamilyStyle ->
  -- | The declarations.
  [LHsDecl GhcPs] ->
  Doc
declRun apart ctx style ds =
  items
    NoBrace
    [ includeWhen gap breakOrSpace <> printed
    | (previous, current) <- zip (Nothing : fmap Just groups) groups,
      let gaps = any (`separate` current) previous : repeat False,
      (gap, (_, printed)) <- zip gaps (NE.toList current)
    ]
  where
    isSignatureFile = ctxSourceType ctx == SignatureSource
    groups =
      fmap (\d -> (d, at_ ctx (hsDecl ctx style) d))
        <$> groupDecls ctx isSignatureFile ds

    separate previous current =
      not (remarkUnder ctx ended began || underHeading)
        && (separatedByBlank ctx ended began || apart previous current)
      where
        ended = spanOf (fst (NE.last previous))
        began = spanOf (fst (NE.head current))
        underHeading =
          isHeading (unLoc (fst (NE.last previous)))
            && commentRightUnder ctx ended began

    isHeading = \case
      DocD _ DocGroup{} -> True
      _ -> False

-- | Gather declarations that belong together.
groupDecls ::
  -- | The context.
  Ctx ->
  -- | Are these the declarations of a signature file?
  Bool ->
  -- | The declarations.
  [LHsDecl GhcPs] ->
  [NonEmpty (LHsDecl GhcPs)]
groupDecls _ _ [] = []
groupDecls ctx isSignatureFile (d : ds)
  | isDocNext (unLoc d) = case groupDecls ctx isSignatureFile ds of
      [] -> [d :| []]
      (g : gs)
        | isDoc (unLoc (NE.head g)) -> (d :| []) : g : gs
        | otherwise -> (d <| g) : gs
  | otherwise =
      let (together, rest) = gather d ds
       in (d :| together) : groupDecls ctx isSignatureFile rest
  where
    gather previous = \case
      current : more
        | belongs previous current ->
            let (together, rest) = gather current more
             in (current : together, rest)
      remaining -> ([], remaining)
    isDocNext = \case
      DocD _ (DocCommentNext _) -> True
      _ -> False
    belongs previous current =
      (not isSignatureFile && isSignatureSeries ctx previous current)
        || isSeries ctx previous current
        || ( relatedDecls d current
               && not (separatedByBlank ctx (spanOf previous) (spanOf current))
           )
        || relatedDecls previous current

-- | A run of type signatures with nothing between them is a list, and a
-- list reads better without gaps in it.
isSignatureSeries :: Ctx -> LHsDecl GhcPs -> LHsDecl GhcPs -> Bool
isSignatureSeries ctx x@(L _ a) y@(L _ b) = case (a, b) of
  (SigD _ TypeSig{}, SigD _ TypeSig{}) ->
    not (commentPrintedBetween ctx (spanOf x) (spanOf y))
  _ -> False

-- | Two standalone @deriving@ declarations, or two splices, the author ran
-- together.
isSeries :: Ctx -> LHsDecl GhcPs -> LHsDecl GhcPs -> Bool
isSeries ctx x@(L _ a) y@(L _ b) = case (a, b) of
  (DerivD{}, DerivD{}) -> ranTogether
  (SpliceD{}, SpliceD{}) -> ranTogether
  _ -> False
  where
    ranTogether = not (separatedByBlank ctx (spanOf x) (spanOf y))

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
  | FixityDeclaration
  | InstanceDeclaration
  | TypeInstance
  | DataInstance
  | DerivingDeclaration
  | SpliceDeclaration
  | ForeignDeclaration
  | RoleAnnotation
  | DefaultDeclaration
  | DocumentsPrevious
  | Unremarkable
  deriving (Eq, Show)

-- | What a declaration is, and what it names.
declKind ::
  -- | The declaration.
  HsDecl GhcPs ->
  (Kind, [RdrName])
declKind = \case
  SigD _ (TypeSig _ ns _) -> (TypeSignature, fmap unLoc ns)
  SigD _ (ClassOpSig _ True ns _) -> (DefaultSignature, fmap unLoc ns)
  SigD _ (ClassOpSig _ False ns _) -> (TypeSignature, fmap unLoc ns)
  SigD _ (PatSynSig _ ns _) -> (PatternSignature, fmap unLoc ns)
  SigD _ (InlineSig _ (L _ n) _) -> (PragmaDeclaration, [n])
  SigD _ (SCCFunSig _ (L _ n) _) -> (PragmaDeclaration, [n])
  SigD _ sig
    | Just n <- specialisedName sig -> (PragmaDeclaration, [n])
  SigD _ CompleteMatchSig{} -> (PragmaDeclaration, [])
  SigD _ (FixSig _ (FixitySig _ ns _)) -> (FixityDeclaration, fmap unLoc ns)
  ValD _ (FunBind _ (L _ n) _) -> (FunctionBody, [n])
  ValD _ (PatBind _ p _ _) -> (FunctionBody, collectPatBinders CollNoDictBinders p)
  ValD _ (PatSynBind _ (PSB _ (L _ n) _ _ _)) -> (PatternDefinition, [n])
  AnnD _ (HsAnnotation _ (ValueAnnProvenance (L _ n)) _) -> (PragmaDeclaration, [n])
  AnnD _ (HsAnnotation _ (TypeAnnProvenance (L _ n)) _) -> (PragmaDeclaration, [n])
  AnnD _ (HsAnnotation _ ModuleAnnProvenance _) -> (PragmaDeclaration, [])
  RuleD{} -> (PragmaDeclaration, [])
  WarningD _ (Warnings _ ws) ->
    (PragmaDeclaration, [unLoc n | L _ (Warning _ ns _) <- ws, n <- ns])
  TyClD _ (DataDecl _ (L _ n) _ _ _) -> (DataDeclaration, [n])
  TyClD _ (ClassDecl{tcdLName = L _ n}) -> (ClassDeclaration, [n])
  TyClD _ (SynDecl _ (L _ n) _ _ _) -> (TypeSynonym, [n])
  TyClD _ (FamDecl _ (FamilyDecl _ _ _ (L _ n) _ _ _ _)) -> (FamilyDeclaration, [n])
  KindSigD _ (StandaloneKindSig _ (L _ n) _) -> (KindSignature, [n])
  InstD _ ClsInstD{} -> (InstanceDeclaration, [])
  InstD _ TyFamInstD{} -> (TypeInstance, [])
  InstD _ DataFamInstD{} -> (DataInstance, [])
  DerivD{} -> (DerivingDeclaration, [])
  SpliceD{} -> (SpliceDeclaration, [])
  ForD{} -> (ForeignDeclaration, [])
  RoleAnnotD{} -> (RoleAnnotation, [])
  DefD{} -> (DefaultDeclaration, [])
  DocD _ (DocCommentPrev _) -> (DocumentsPrevious, [])
  _ -> (Unremarkable, [])

-- | Is this a Haddock?
isDoc ::
  -- | The declaration.
  HsDecl GhcPs ->
  Bool
isDoc = \case
  DocD{} -> True
  _ -> False

-- | Should these two declarations be printed with no blank line between
-- them?
relatedDecls :: LHsDecl GhcPs -> LHsDecl GhcPs -> Bool
relatedDecls a b = case (kindA, kindB) of
  (_, DocumentsPrevious) -> True
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
shareAName xs ys = overlaps (sort (fmap spelling xs)) (sort (fmap spelling ys))
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

-- | A type, class or family declaration.
tyClDecl :: Ctx -> FamilyStyle -> TyClDecl GhcPs -> Doc
tyClDecl ctx style = \case
  FamDecl _ x -> famDecl ctx style x
  SynDecl{..} -> synDecl ctx tcdLName tcdFixity tcdTyVars tcdRhs
  DataDecl{..} ->
    dataDecl
      ctx
      (Don't #instanceWord)
      tcdLName
      (hsq_explicit tcdTyVars)
      spanOf
      (at_ ctx (tyVarBndr ctx))
      tcdFixity
      mempty
      tcdDataDefn
  ClassDecl{tcdCExt = (anns, _, _), ..} ->
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

-- | An instance of a class, or of a type or data family.
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
    <> indent (parens (commaSep (fmap (align . hsType ctx) types)))

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
  fd@ForeignImport{fd_fi} -> foreignImport ctx fd_fi <> foreignSig ctx fd
  fd@ForeignExport{fd_fe} -> foreignExport ctx fd_fe <> foreignSig ctx fd

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

-- | The head of a foreign export.
foreignExport :: Ctx -> ForeignExport GhcPs -> Doc
foreignExport ctx (CExport src (L loc (CExportStatic _ _ callConv))) =
  txt "foreign export"
    <> space
    <> at ctx (L loc callConv) outputable
    <> space
    <> at ctx src sourceText
