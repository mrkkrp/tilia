{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Expressions, and the equations and blocks built out of them.
module Tilia.Render.Expression
  ( hsExpr,
    hsExprIn,
    hsCmd,
    valDecl,
    MatchStyle (..),
    GuardStyle (..),
    untypedSplice,
  )
where

import Data.Function (on)
import Data.Generics.Schemes (listify)
import Data.List (sortBy, unsnoc)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (unpackFS)
import GHC.Hs hiding (Fixity)
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.Basic (Boxity (..))
import GHC.Types.Fixity (LexicalFixity (..))
import GHC.Types.Name.Occurrence (isVarOcc)
import GHC.Types.Name.Reader (RdrName, mkVarUnqual, rdrNameOcc)
import GHC.Types.SourceText
import GHC.Types.SrcLoc
  ( GenLocated (..),
    isZeroWidthSpan,
    leftmost_smallest,
    noSrcSpan,
    unLoc,
  )
import Language.Haskell.Syntax.Basic (field_label)
import Tilia.Doc.Body
import Tilia.Doc.Combinators
import Tilia.Fixity
  ( Fixity,
    Namespace (..),
  )
import Tilia.Render.Body
import Tilia.Render.Context
import Tilia.Render.Layout
import Tilia.Render.Literal (stringLiteral)
import Tilia.Render.Name
import Tilia.Render.Operator
import Tilia.Render.Pattern
import Tilia.Render.Type
import Tilia.Span
import Tilia.Span.Ghc

-- | An expression.
hsExpr :: Ctx -> LHsExpr GhcPs -> Doc
hsExpr ctx = hsExprIn ctx plainSite

-- | An expression that knows where it stands.
hsExprIn :: Ctx -> Site -> LHsExpr GhcPs -> Doc
hsExprIn ctx site l = at ctx l (exprBody ctx site (spanOf l))

exprBody :: Ctx -> Site -> Maybe Span -> HsExpr GhcPs -> Doc
exprBody ctx site here = \case
  HsVar _ n -> name ctx n
  HsOverLabel src _ -> txt "#" <> sourceText src
  HsIPVar _ (HsIPName n) -> txt "?" <> outputable n
  HsOverLit _ v -> outputable (ol_val v)
  HsLit _ lit -> case lit of
    HsString (SourceText s) _ -> stringLiteral s
    HsStringPrim (SourceText s) _ -> stringLiteral s
    HsMultilineString (SourceText s) _ -> stringLiteral s
    other -> outputable other
  HsLam _ variant' mg -> lambda ctx site variant' (ExprBody ctx) mg
  HsApp _ f x -> application ctx site f x
  HsAppType at' e a ->
    hsExpr ctx e
      <> breakOrSpace
      <> indent
        ( atSpan
            ctx
            (tokenSpan at' <> spanOf (hswc_body a))
            (txt "@" <> hsTypeBody ctx (spanOf (hswc_body a)) (unLoc (hswc_body a)))
        )
  OpApp _ x op y -> exprChain ctx site x op y
  NegApp _ e _ -> txt "-" <> negationGap ctx e <> hsExpr ctx e
  HsPar _ e ->
    layoutWithin ctx here (spanOf e) $
      parensWith (closingFor site) (insideBrackets here (hsExpr ctx e))
  SectionL _ x op -> hsExpr ctx x <> breakOrSpace <> indent (hsExpr ctx op)
  SectionR _ op x -> hsExpr ctx op <> breakOrSpace <> indent (hsExpr ctx x)
  ExplicitTuple _ args boxity -> tuple ctx here (closingFor site) boxity args
  ExplicitSum _ tag arity e -> unboxedSum (closingFor site) tag arity (hsExpr ctx e)
  HsCase _ e mg -> caseOf ctx site (ExprBody ctx) e mg
  HsIf anns c t e -> ifThenElse ctx (bodyIn (ExprBody ctx) (siteBracing site)) anns c t e
  HsMultiIf _ guards ->
    txt "if"
      <> breakOrSpace
      <> underSite site (sepBy breakOrSpace (map alternative (NE.toList guards)))
    where
      alternative g =
        atSpan
          ctx
          (grhsSpan (unLoc g))
          (guardedRhs ctx Normal (siteBracing site) (ExprBody ctx) RightArrow (unLoc g))
  HsLet _ binds e -> letIn ctx (bodyIn (ExprBody ctx) (siteBracing site)) binds e
  HsDo anns flavour es -> case flavour of
    DoExpr moduleName -> doBlock moduleName "do"
    MDoExpr moduleName -> doBlock moduleName "mdo"
    ListComp -> comprehension ctx site es
    MonadComp -> comprehension ctx site es
    GhciStmtCtxt -> error "Tilia: GhciStmtCtxt cannot occur in a source file"
    where
      doBlock moduleName word =
        foldMap (\m -> outputable m <> txt ".") moduleName
          <> keywordAt ctx (doKeywordSpan anns) word
          <> statements ctx site (ExprBody ctx) es
  ExplicitList _ xs ->
    bracketsWith
      (closingFor site)
      (insideBrackets here (commaSep (map (align . hsExpr ctx) xs)))
  RecordCon {..} ->
    name ctx rcon_con
      <> breakOrSpace
      <> indent (braces (insideBrackets here (commaSep (map align (fields <> wildcard)))))
    where
      HsRecFields {..} = rcon_flds
      fields = map (at_ ctx (fieldBind ctx (at_ ctx (name ctx . foLabel)))) rec_flds
      -- The @..@ has a location of its own, and needs it: a comment written
      -- against it has nothing else to attach to.
      wildcard = case rec_dotdot of
        Just l -> [at ctx l (const (txt ".."))]
        Nothing -> []
  RecordUpd {..} ->
    hsExpr ctx rupd_expr <> breakOrSpace <> indent (braces (insideBrackets here updates))
    where
      updates = case rupd_flds of
        RegularRecUpdFields {..} ->
          commaSep (map (align . at_ ctx (fieldBind ctx (at_ ctx (fieldOcc ctx)))) recUpdFields)
        OverloadedRecUpdFields {..} ->
          commaSep (map (align . at_ ctx (fieldBind ctx (at_ ctx labelChain))) olRecUpdFields)
      labelChain (FieldLabelStrings flss) = dotFields ctx (unLoc <$> flss)
  HsGetField {..} ->
    hsExpr ctx gf_expr <> txt "." <> at ctx gf_field (dotField ctx)
  HsProjection {..} -> parens (txt "." <> dotFields ctx proj_flds)
  ExprWithTySig _ x HsWC {hswc_body} ->
    align $
      hsExpr ctx x
        <> joinedBy "::"
        <> indent (hsSigType ctx hswc_body)
  ArithSeq _ _ range -> arithSeq ctx (closingFor site) range
  HsTypedBracket (bracketAnn, _) e ->
    txt opener <> breakOrNothing <> hsExpr ctx e <> breakOrNothing <> txt "||]"
    where
      -- @[e|| … ||]@ and @[|| … ||]@ are the same bracket written two ways,
      -- and which one the author reached for is theirs to keep.
      opener = case bracketAnn of
        BracketNoE {} -> "[||"
        BracketHasE {} -> "[e||"
  HsUntypedBracket _ q -> quotation ctx q
  HsTypedSplice _ (HsTypedSpliceExpr _ e) -> spliceTH ctx True e DollarSplice
  HsUntypedSplice _ splice -> untypedSplice ctx DollarSplice splice
  HsProc _ p e ->
    txt "proc"
      <> layoutFrom ctx (spanOf p) (breakOrSpace <> indent (hsPat ctx p) <> breakOrSpace)
      <> txt "->"
      <> attachBody (CmdTopBody ctx plainSite e)
  HsStatic _ e -> txt "static" <> breakOrSpace <> indent (hsExpr ctx e)
  HsPragE _ prag x -> case prag of
    HsPragSCC _ n ->
      txt "{-# SCC "
        <> outputable n
        <> txt " #-}"
        <> breakOrSpace
        <> nest (if siteInBlock site then 1 else 0) (hsExpr ctx x)
  HsEmbTy _ HsWC {hswc_body} -> txt "type" <> space <> hsType ctx hswc_body
  HsHole holeKind -> case holeKind of
    HoleVar n -> name ctx n
    HoleError -> error "Tilia: a nameless hole cannot come from a successful parse"
  -- The three that follow mirror their counterparts in types: a quoted
  -- signature is an expression until it is elaborated.
  HsForAll _ tele e -> forallTelescope ctx tele <> breakOrSpace <> hsExpr ctx e
  HsQual _ qs e ->
    at ctx qs (contextOf loneVariableExpr (hsExpr ctx) . map unbracketed)
      <> joinedBy "=>"
      <> hsExpr ctx e
  HsFunArr _ multAnn x y ->
    hsExpr ctx x
      <> space
      <> multiplicity (hsExpr ctx) multAnn
      <> joinedBy "->"
      <> case unLoc y of
        HsFunArr {} -> exprBody ctx plainSite (spanOf y) (unLoc y)
        _ -> hsExpr ctx y

-- | The body of an equation, as the enclosing construct will hand it on.
type BodyOf body b = Site -> LocatedA body -> b

-- | A body standing on its own, with the given bracing.
bodyIn :: BodyOf body b -> Bracing -> LocatedA body -> b
bodyIn mkBody bracing = mkBody (withBracing bracing plainSite)

-- | @-@ in front of a literal needs a space when @NegativeLiterals@ is on,
-- since @- 1@ and @-1@ then parse differently.
negationGap :: Ctx -> LHsExpr GhcPs -> Doc
negationGap ctx e =
  includeWhen (extensionOn ctx NegativeLiterals && isLiteral) space
  where
    isLiteral = case unLoc e of
      HsLit {} -> True
      HsOverLit {} -> True
      _ -> False

-- | A function applied to arguments.
application :: Ctx -> Site -> LHsExpr GhcPs -> LHsExpr GhcPs -> Doc
application ctx site f x =
  case placement of
    Normal ->
      whenFlat (siteBracing site) headAndInit
        <> indent (includeUnless (null initArgs) breakOrSpace <> hsExpr ctx lastArg)
    Hanging ->
      layoutFrom ctx initSpan (headAndInit MayBrace)
        <> attach Hanging (hsExpr ctx lastArg)
  where
    (func, args) = gatherArgs f (x :| [])
    initArgs = NE.init args
    lastArg = NE.last args
    initSpan = spanOf f <> (startOf <$> spanOf lastArg)
    placement
      | maybe False isSingleLine initSpan = exprHangs (unLoc lastArg)
      | otherwise = Normal
    headAndInit bracing =
      hsExprIn ctx site {siteApplicand = True, siteBracing = bracing} func
        <> breakOrSpace
        <> nest
          (if placement == Hanging then 0 else 1)
          ( sepBy
              breakOrSpace
              (map (hsExprIn ctx (withBracing bracing plainSite)) initArgs)
          )

gatherArgs ::
  LHsExpr GhcPs ->
  NonEmpty (LHsExpr GhcPs) ->
  (LHsExpr GhcPs, NonEmpty (LHsExpr GhcPs))
gatherArgs f known = case unLoc f of
  HsApp _ l r -> gatherArgs l (NE.cons r known)
  _ -> (f, known)

-- | A tuple, or a tuple section.
tuple :: Ctx -> Maybe Span -> ClosingIndent -> Boxity -> [HsTupArg GhcPs] -> Doc
tuple ctx here closing boxity args
  | any isMissing args = flat (brackets' (sepBy comma (map arg args)))
  | otherwise = brackets' (insideBrackets here (commaSep (map arg args)))
  where
    brackets' = case boxity of
      Boxed -> parensWith closing
      Unboxed -> unboxedWith closing
    isMissing = \case
      Missing _ -> True
      _ -> False
    arg =
      align . \case
        Present _ e -> hsExpr ctx e
        Missing _ -> mempty

-- | An arithmetic sequence, in whichever of its four forms.
arithSeq :: Ctx -> ClosingIndent -> ArithSeqInfo GhcPs -> Doc
arithSeq ctx closing = \case
  From from -> wrap (hsExpr ctx from <> breakOrSpace <> txt "..")
  FromThen from next ->
    wrap (commaSep (map (hsExpr ctx) [from, next]) <> breakOrSpace <> txt "..")
  FromTo from to ->
    wrap (hsExpr ctx from <> breakOrSpace <> txt ".." <> space <> hsExpr ctx to)
  FromThenTo from next to ->
    wrap $
      commaSep (map (hsExpr ctx) [from, next])
        <> breakOrSpace
        <> txt ".."
        <> space
        <> hsExpr ctx to
  where
    wrap = bracketsWith closing

-- | One field of a record construction or update.
fieldBind ::
  (HasLoc l) =>
  Ctx ->
  (GenLocated l a -> Doc) ->
  HsFieldBind (GenLocated l a) (LHsExpr GhcPs) ->
  Doc
fieldBind ctx label HsFieldBind {..} =
  label hfbLHS
    <> includeUnless hfbPun (space <> txt "=" <> attach placement (hsExpr ctx hfbRHS))
  where
    placement
      | sameLine (spanOf hfbLHS) (spanOf hfbRHS) = exprHangs (unLoc hfbRHS)
      | otherwise = Normal

dotField :: Ctx -> DotFieldOcc GhcPs -> Doc
dotField ctx = name ctx . fmap (mkVarUnqual . field_label) . dfoLabel

dotFields :: Ctx -> NonEmpty (DotFieldOcc GhcPs) -> Doc
dotFields ctx = sepBy (txt ".") . map (dotField ctx) . NE.toList

-- | A chain of operators applied to expressions.
exprChain ::
  Ctx ->
  Site ->
  LHsExpr GhcPs ->
  LHsExpr GhcPs ->
  LHsExpr GhcPs ->
  Doc
exprChain ctx site x op y =
  renderExprChain ctx site (uncurry (associate (fixityOf ctx)) chain)
  where
    chain = flattenAround splitOpApp x op y

splitOpApp ::
  LHsExpr GhcPs ->
  Maybe (LHsExpr GhcPs, LHsExpr GhcPs, LHsExpr GhcPs)
splitOpApp e = case unLoc e of
  OpApp _ l o r -> Just (l, o, r)
  _ -> Nothing

fixityOf :: Ctx -> LHsExpr GhcPs -> Maybe Fixity
fixityOf ctx o = operatorName o >>= operatorFixity ctx InTerms

renderExprChain :: Ctx -> Site -> OpChain (LHsExpr GhcPs) (LHsExpr GhcPs) -> Doc
renderExprChain ctx site = \case
  Operand e -> hsExprIn ctx site e
  chain@(Chain operands@(firstOne :| rest) operators) ->
    layoutFrom ctx (chainSpan spanOf chain) $
      if trailing
        then laidOut MayBrace
        else whenFlat (if placement == Hanging then MayBrace else NoBrace) laidOut
    where
      placement = chainPlacement exprHangs firstOne (NE.last operands)
      laidOut bracing =
        renderExprChain ctx (withBracing bracing site) firstOne
          <> pieces bracing firstOne operators rest
      pieces bracing previous (o : os) (operand : more) =
        let isLast = null more
            operandSite =
              withBracing (if isLast then siteBracing site else bracing) plainSite
            rendered = renderExprChain ctx operandSite operand
            rest' = pieces bracing operand os more
         in if trailing
              then space <> hsExpr ctx o <> attach (tailPlacement isLast previous operand) (rendered <> rest')
              else attach placement (hsExpr ctx o <> space <> rendered) <> rest'
      pieces _ _ _ _ = mempty
      tailPlacement isLast previous operand
        | isLast,
          not (maybe True isSingleLine (chainSpan spanOf operand)) =
            chainPlacement exprHangs previous operand
        | otherwise = Normal
      commentedOperators =
        or (zipWith commentedBefore (NE.toList operands) operators)
      commentedBefore operand o =
        commentBetween ctx (chainSpan spanOf operand) (spanOf o)
      trailing =
        (length operators == 1 || endsHanging)
          && not commentedOperators
          && and (zipWith couldTrail (NE.toList operands) operators)
      endsHanging = exprHangs (unLoc (lastOperand chain)) == Hanging
      couldTrail previous o =
        isSeparator (fixityOf ctx o)
          && maybe False isSingleLine (chainSpan spanOf previous)
          && placement == Normal
          && not (isDoBlock (lastOperand previous))

isDoBlock :: LHsExpr GhcPs -> Bool
isDoBlock e = case unLoc e of
  HsDo _ (DoExpr _) _ -> True
  HsDo _ (MDoExpr _) _ -> True
  _ -> False

-- | Whether the operands of a chain hang.
chainPlacement ::
  (HasLoc l) =>
  (a -> Placement) ->
  OpChain (GenLocated l a) op ->
  OpChain (GenLocated l a) op ->
  Placement
chainPlacement placer firstOne lastOne = case lastOne of
  Operand (L _ n) | startsTogether -> placer n
  _ -> Normal
  where
    startsTogether =
      case (chainSpan spanOf firstOne, chainSpan spanOf lastOne) of
        (Just a, Just b) -> spanStartLine a == spanStartLine b
        _ -> False

-- | Is this quoted constraint nothing but a variable?
loneVariableExpr :: LHsExpr GhcPs -> Bool
loneVariableExpr e = case unLoc e of
  HsVar _ (L _ n) -> isVarOcc (rdrNameOcc n)
  _ -> False

unbracketed :: LHsExpr GhcPs -> LHsExpr GhcPs
unbracketed e = case unLoc e of
  HsPar _ inner -> unbracketed inner
  _ -> e

-- | A statement of a comprehension or a guard.
hsStmt :: Ctx -> ExprLStmt GhcPs -> Doc
hsStmt ctx = at_ ctx (stmtBody ctx plainSite (ExprBody ctx))

stmtBody ::
  ( Body b,
    Anno [LStmt GhcPs (XRec GhcPs body)] ~ SrcSpanAnnLW,
    Anno (Stmt GhcPs (XRec GhcPs body)) ~ SrcSpanAnnA,
    Anno body ~ SrcSpanAnnA
  ) =>
  Ctx ->
  Site ->
  BodyOf body b ->
  Stmt GhcPs (XRec GhcPs body) ->
  Doc
stmtBody ctx site mkBody = \case
  LastStmt _ body _ _ -> printBody (mkBody site body)
  BodyStmt _ body _ _ -> printBody (mkBody site body)
  BindStmt _ p f ->
    hsPat ctx p
      <> nest 1 (space <> txt "<-")
      <> layoutFrom ctx (spanOf p <> spanOf f) (attach placement (printBody bound))
    where
      bound = mkBody plainSite f
      placement
        | sameLine (spanOf p) (spanOf f) = bodyPlacement bound
        | otherwise = Normal
  LetStmt _ binds -> txt "let" <> space <> align (bound binds)
    where
      bound = \case
        EmptyLocalBinds _ -> txt "{}"
        bs -> localBinds ctx (siteBracing site) bs
  ParStmt {} ->
    error "Tilia: ParStmt should have been unpacked"
  TransStmt {..} -> case (trS_form, trS_by) of
    (ThenForm, Nothing) ->
      txt "then" <> breakOrSpace <> indent (hsExpr ctx trS_using)
    (ThenForm, Just e) ->
      txt "then"
        <> breakOrSpace
        <> indent (hsExpr ctx trS_using)
        <> breakOrSpace
        <> txt "by"
        <> breakOrSpace
        <> indent (hsExpr ctx e)
    (GroupForm, Nothing) ->
      txt "then group using" <> breakOrSpace <> indent (hsExpr ctx trS_using)
    (GroupForm, Just e) ->
      txt "then group by"
        <> breakOrSpace
        <> indent (hsExpr ctx e)
        <> breakOrSpace
        <> txt "using"
        <> breakOrSpace
        <> indent (hsExpr ctx trS_using)
  RecStmt {..} ->
    txt "rec"
      <> space
      <> align
        ( at ctx recS_stmts $ \xs ->
            items (siteBracing site) $
              keepBlanks
                (separatedByBlank ctx)
                [ (spanOf s, at_ ctx (stmtBody ctx site mkBody) s)
                | s <- xs
                ]
        )

-- | The statements of a block, with the break that introduces them.
statements ::
  ( Body b,
    Anno [LStmt GhcPs (XRec GhcPs body)] ~ SrcSpanAnnLW,
    Anno (Stmt GhcPs (XRec GhcPs body)) ~ SrcSpanAnnA,
    Anno body ~ SrcSpanAnnA
  ) =>
  Ctx ->
  Site ->
  BodyOf body b ->
  XRec GhcPs [LStmt GhcPs (XRec GhcPs body)] ->
  Doc
statements ctx site mkBody es =
  breakOrSpace <> underSite site (at ctx es block)
  where
    block xs =
      items (siteBracing site) $
        keepBlanks
          (separatedByBlank ctx)
          [(spanOf stmt, item place stmt) | (place, stmt) <- places xs]
    item place stmt = case place of
      Last -> rendered (siteBracing site)
      Only -> rendered (siteBracing site)
      _ -> whenFlat (siteBracing site) rendered
      where
        rendered bracing =
          at_ ctx (stmtBody ctx (blockSite bracing) mkBody) stmt
    blockSite bracing =
      plainSite {siteInBlock = True, siteBracing = bracing}

-- | A list comprehension.
comprehension :: Ctx -> Site -> XRec GhcPs [ExprLStmt GhcPs] -> Doc
comprehension ctx site es = align (variant onOneLine acrossLines)
  where
    onOneLine = txt "[" <> body <> txt "]"
    acrossLines = txt "[" <> space <> keepInside (body <> hardBreak <> txt "]")
    keepInside = if siteInBlock site then align else id
    body = at ctx es sections
    sections xs = case unsnoc xs of
      Nothing -> error "Tilia: a comprehension always yields something"
      Just (stmts, yield) ->
        align (hsStmt ctx yield)
          <> breakOrSpace
          <> sepBy breakOrSpace (map section (comprehensionSections stmts))
    section stmts =
      located'
        (spansOf stmts)
        (txt "|" <> space <> align (commaSep (map (align . hsStmt ctx) stmts)))
    located' = maybe id located

-- | Split the statements of a comprehension into its parallel sections.
comprehensionSections :: [ExprLStmt GhcPs] -> [[ExprLStmt GhcPs]]
comprehensionSections = map unnest . branches
  where
    branches = \case
      [L _ (ParStmt _ blocks _ _)] ->
        [run | ParStmtBlock _ run _ _ <- NE.toList blocks]
      run -> [run]
    unnest = concatMap $ \case
      L _ ParStmt {} -> error "Tilia: parallel blocks do not nest"
      stmt@(L _ TransStmt {trS_stmts}) -> unnest trS_stmts <> [stmt]
      stmt -> [stmt]

-- | A @case@ expression or command.
caseOf ::
  ( Body b,
    Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  Ctx ->
  Site ->
  BodyOf body b ->
  LHsExpr GhcPs ->
  MatchGroup GhcPs (LocatedA body) ->
  Doc
caseOf ctx site mkBody scrutinee mg =
  txt "case"
    <> space
    <> hsExpr ctx scrutinee
    <> joinedBy "of"
    <> bracedForSite site alternatives
  where
    alternatives b = underSite site (matchGroup ctx b mkBody CaseStyle mg)

-- | A lambda, in any of its three spellings.
lambda ::
  ( Body b,
    Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  Ctx ->
  Site ->
  HsLamVariant ->
  BodyOf body b ->
  MatchGroup GhcPs (LocatedA body) ->
  Doc
lambda ctx site variant' mkBody mg = case keyword of
  Nothing -> matchGroup ctx (siteBracing site) mkBody LambdaStyle mg
  Just kw -> txt kw <> breakOrSpace <> bracedForSite site alternatives
  where
    alternatives b = underSite site (matchGroup ctx b mkBody LambdaCaseStyle mg)
    keyword = case variant' of
      LamSingle -> Nothing
      LamCase -> Just "\\case"
      LamCases -> Just "\\cases"

-- | An @if@ expression or command.
ifThenElse ::
  (Body b) =>
  Ctx ->
  (LocatedA body -> b) ->
  AnnsIf ->
  LHsExpr GhcPs ->
  LocatedA body ->
  LocatedA body ->
  Doc
ifThenElse ctx bodyOf AnnsIf {aiThen, aiElse} condition thenBody elseBody =
  txt "if"
    <> space
    <> hsExpr ctx condition
    <> breakOrSpace
    <> indent
      ( branch (locA aiThen) "then" thenBody
          <> breakOrSpace
          <> branch (locA aiElse) "else" elseBody
      )
  where
    branch written word body =
      keywordAt ctx keywordSpan word
        <> space
        <> layoutFrom
          ctx
          (keywordSpan <> spanOf body)
          (attach placement (printBody (bodyOf body)))
      where
        keywordSpan = spanOfSrcSpan written
        placement
          | commentBetween ctx keywordSpan (spanOf body) = Normal
          | otherwise = bodyPlacement (bodyOf body)

-- | A @let@ expression or command.
letIn ::
  (Body b) =>
  Ctx ->
  (LocatedA body -> b) ->
  HsLocalBinds GhcPs ->
  LocatedA body ->
  Doc
letIn ctx bodyOf binds body =
  align $
    -- Neither keyword claims its span, unlike the @do@ of a block. Both
    -- have something printed after them on their own line—the first
    -- binding, the body—so a comment either of them claimed would be held
    -- back over that and come out against it, which is further from where
    -- it was written than where it lands by falling through.
    txt "let"
      <> space
      <> align (localBinds ctx NoBrace binds)
      <> variant space (hardBreak <> txt " ")
      <> txt "in"
      <> space
      <> align (printBody (bodyOf body))

-- | The bindings of a @let@ or a @where@.
localBinds :: Ctx -> Bracing -> HsLocalBinds GhcPs -> Doc
localBinds ctx bracing = \case
  HsValBinds ann (ValBinds _ binds sigs) ->
    anchored ann . align . items bracing $
      keepBlanks
        (separatedByBlank ctx)
        [(spanOf item, rendered place item) | (place, item) <- places sorted]
    where
      sorted =
        sortBy
          (leftmost_smallest `on` getLocA)
          (map (fmap Left) binds <> map (fmap Right) sigs)
      rendered place item = case place of
        Last -> render NoBrace
        Only -> render NoBrace
        _ -> whenFlat NoBrace render
        where
          render b =
            at_ ctx (either (valDecl ctx b) (knotSig (ctxKnot ctx) ctx)) item
  HsValBinds _ _ -> error "Tilia: renamer-only local bindings"
  HsIPBinds ann (IPBinds _ xs) ->
    anchored ann (items bracing (map (at_ ctx implicitBind) xs))
  EmptyLocalBinds _ -> mempty
  where
    implicitBind (IPBind _ (L _ n) e) =
      outputable n
        <> joinedBy "="
        <> indent (hsExprIn ctx (withBracing MayBrace plainSite) e)
    anchored ann d = case ann of
      EpAnn {anns = AnnList {al_anchor}}
        | not (isZeroWidthSpan (locA al_anchor)) ->
            atSpan ctx (spanOfSrcSpan (locA al_anchor)) d
      _ -> d

-- | Where a @do@ or @mdo@ was written.
doKeywordSpan :: AnnList EpaLocation -> Maybe Span
doKeywordSpan = spanOfSrcSpan . locA . al_rest

-- | Where the @where@ keyword of a group of local bindings was.
whereKeywordSpan :: HsLocalBinds GhcPs -> Maybe Span
whereKeywordSpan =
  spanOfSrcSpan . \case
    HsValBinds EpAnn {anns = AnnList {al_rest}} _ -> locA al_rest
    HsIPBinds EpAnn {anns = AnnList {al_rest}} _ -> locA al_rest
    EmptyLocalBinds _ -> noSrcSpan

isEmptyLocalBinds :: HsLocalBinds GhcPs -> Bool
isEmptyLocalBinds = \case
  EmptyLocalBinds _ -> True
  HsValBinds _ (ValBinds _ binds sigs) -> null binds && null sigs
  _ -> False

-- | An arrow-notation command.
hsCmd :: Ctx -> Site -> LHsCmd GhcPs -> Doc
hsCmd ctx site l = at ctx l (cmdBody ctx site)

cmdBody :: Ctx -> Site -> HsCmd GhcPs -> Doc
cmdBody ctx site = \case
  HsCmdArrApp _ body input arrow rightToLeft ->
    let writtenFirst = if rightToLeft then body else input
        writtenSecond = if rightToLeft then input else body
     in hsExprIn ctx site {siteApplicand = False} writtenFirst
          <> breakOrSpace
          <> indent
            ( txt (arrowText arrow rightToLeft)
                <> attach (exprHangs (unLoc input)) (hsExpr ctx writtenSecond)
            )
  HsCmdArrForm _ form Prefix cmds ->
    bananaWith (closingFor site) $
      hsExpr ctx form
        <> includeUnless
          (null cmds)
          (breakOrSpace <> indent (sepBy breakOrSpace (map (printBody . CmdTopBody ctx plainSite) cmds)))
  HsCmdArrForm _ form Infix [l, r] -> cmdChain ctx site l form r
  HsCmdArrForm _ _ Infix _ ->
    error "Tilia: an infix command form always has exactly two operands"
  HsCmdApp _ cmd e ->
    hsCmd ctx site {siteApplicand = True} cmd
      <> breakOrSpace
      <> indent (hsExpr ctx e)
  HsCmdLam _ variant' mg -> lambda ctx site variant' (CmdBody ctx) mg
  HsCmdPar _ c -> parens (hsCmd ctx plainSite c)
  HsCmdCase _ e mg -> caseOf ctx site (CmdBody ctx) e mg
  HsCmdIf anns _ c t e ->
    ifThenElse ctx (bodyIn (CmdBody ctx) (siteBracing site)) anns c t e
  HsCmdLet _ binds c -> letIn ctx (bodyIn (CmdBody ctx) (siteBracing site)) binds c
  HsCmdDo anns es ->
    keywordAt ctx (doKeywordSpan anns) "do"
      <> statements ctx site (CmdBody ctx) es

arrowText :: HsArrAppType -> Bool -> Text
arrowText arrow rightToLeft = case (arrow, rightToLeft) of
  (HsFirstOrderApp, True) -> "-<"
  (HsHigherOrderApp, True) -> "-<<"
  (HsFirstOrderApp, False) -> ">-"
  (HsHigherOrderApp, False) -> ">>-"

-- | A command at the top of an arrow form.
cmdTop :: Ctx -> Site -> HsCmdTop GhcPs -> Doc
cmdTop ctx site (HsCmdTop _ cmd) = hsCmd ctx site cmd

-- | A chain of operators applied to commands.
cmdChain ::
  Ctx ->
  Site ->
  LHsCmdTop GhcPs ->
  LHsExpr GhcPs ->
  LHsCmdTop GhcPs ->
  Doc
cmdChain ctx site l op r =
  render (uncurry (associate (fixityOf ctx)) (flattenAround splitCmd l op r))
  where
    splitCmd c = case unLoc c of
      HsCmdTop _ (L _ (HsCmdArrForm _ o Infix [a, b])) -> Just (a, o, b)
      _ -> Nothing
    render = \case
      Operand c -> at ctx c (cmdTop ctx site)
      chain@(Chain operands@(firstOne :| rest) operators) ->
        layoutFrom ctx (chainSpan spanOf chain) $
          whenFlat (if placement == Hanging then MayBrace else NoBrace) laidOut
        where
          placement = chainPlacement cmdTopHangs firstOne (NE.last operands)
          laidOut bracing =
            renderIn bracing firstOne <> pieces bracing operators rest
          pieces bracing (o : os) (operand : more) =
            attach
              placement
              (hsExpr ctx o <> space <> renderIn (if null more then siteBracing site else bracing) operand)
              <> pieces bracing os more
          pieces _ _ _ = mempty
          renderIn bracing = \case
            Operand c -> at ctx c (cmdTop ctx (withBracing bracing site))
            inner -> render inner

-- | A value binding.
valDecl :: Ctx -> Bracing -> HsBind GhcPs -> Doc
valDecl ctx bracing = \case
  FunBind _ funId funMatches ->
    matchGroup ctx bracing (ExprBody ctx) (FunctionStyle funId) funMatches
  PatBind _ p multAnn grhss ->
    match ctx bracing (ExprBody ctx) PatternBindStyle False multAnn NoSrcStrict [p] grhss
  PatSynBind _ psb -> patSynBind ctx psb
  VarBind {} -> error "Tilia: VarBind is introduced by the type checker"

-- | Which shape a group of equations takes.
data MatchStyle
  = -- | @f x = …@
    FunctionStyle (LocatedN RdrName)
  | -- | @(x, y) = …@
    PatternBindStyle
  | -- | An alternative of a @case@
    CaseStyle
  | -- | The body of a @\\@
    LambdaStyle
  | -- | An alternative of a @\\case@ or @\\cases@
    LambdaCaseStyle

-- | What separates a guard from what it guards.
data GuardStyle
  = EqualsSign
  | RightArrow
  deriving (Eq, Show)

-- | A group of equations.
matchGroup ::
  ( Body b,
    Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  Ctx ->
  Bracing ->
  BodyOf body b ->
  MatchStyle ->
  MatchGroup GhcPs (LocatedA body) ->
  Doc
matchGroup ctx bracing mkBody style MG {..} =
  items blockBracing (map rendered (places (unLoc mg_alts)))
  where
    blockBracing = case style of
      CaseStyle -> ifEmpty
      LambdaCaseStyle -> ifEmpty
      _ -> NoBrace
    ifEmpty = if null (unLoc mg_alts) then MayBrace else bracing
    rendered (place, m) = case place of
      Last -> written bracing
      Only -> written bracing
      _ -> whenFlat bracing written
      where
        written b = at_ ctx (renderMatch b) m
    renderMatch b m@Match {..} =
      match
        ctx
        b
        mkBody
        (adjustStyle m style)
        (isInfixMatch m)
        (HsUnannotated EpPatBind)
        (bangBeforeName m)
        (unLoc m_pats)
        m_grhss

-- | Put this equation's own name into the style it is printed with. A
-- style that carries no name is left as it is.
adjustStyle :: Match GhcPs body -> MatchStyle -> MatchStyle
adjustStyle m = \case
  FunctionStyle _ | FunRhs {mc_fun = f} <- m_ctxt m -> FunctionStyle f
  style -> style

-- | Was a @!@ written in front of the name this equation defines?
bangBeforeName :: Match id body -> SrcStrictness
bangBeforeName = \case
  Match {m_ctxt = FunRhs {mc_strictness}} -> mc_strictness
  _ -> NoSrcStrict

-- | One equation: a head, a body, and possibly a @where@.
match ::
  (Body b, Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO) =>
  Ctx ->
  Bracing ->
  BodyOf body b ->
  MatchStyle ->
  -- | Written infix?
  Bool ->
  HsMultAnn GhcPs ->
  SrcStrictness ->
  [LPat GhcPs] ->
  GRHSs GhcPs (LocatedA body) ->
  Doc
match ctx bracing mkBody style isInfix multAnn strict pats GRHSs {..} =
  multiplicity (hsType ctx) multAnn
    <> multAnnGap
    <> strictness strict
    <> head'
    <> nest
      (if indentBody then 1 else 0)
      ( separator
          <> layoutFrom ctx bodySpan (attach placement body)
          <> indent whereClause
      )
  where
    multAnnGap = case multAnn of
      HsUnannotated {} -> mempty
      _ -> space

    indentBody = case pats of
      [] -> False
      _ ->
        not (maybe True isSingleLine headSpan)
          && not (isCaseStyle style && any containsOrPat pats)

    headSpan = case style of
      FunctionStyle n -> spanOf n <> patSpans
      _ -> patSpans
    patSpans = spansOf pats

    head' = case pats of
      [] -> case style of
        FunctionStyle n -> name ctx n
        _ -> mempty
      (headPat : tailPats) -> layoutFrom ctx headSpan $ case style of
        FunctionStyle n -> defHead isInfix indentBody (name ctx n) rendered
        PatternBindStyle -> sepBy breakOrSpace rendered
        CaseStyle -> sepBy breakOrSpace rendered
        LambdaStyle -> txt "\\" <> lambdaGap headPat <> align (sepBy breakOrSpace rendered)
        LambdaCaseStyle ->
          hsPat ctx headPat
            <> includeUnless
              (null tailPats)
              (breakOrSpace <> indent (sepBy breakOrSpace (map (hsPat ctx) tailPats)))
      where
        rendered = map (hsPat ctx) pats

    lambdaGap p = includeWhen (needsGap (unLoc p)) space
    needsGap = \case
      LazyPat {} -> True
      BangPat {} -> True
      SplicePat {} -> True
      InvisPat {} -> True
      _ -> False

    endOfPats = case pats of
      [] -> case style of
        FunctionStyle n -> spanOf n
        _ -> Nothing
      _ -> spanOf (last pats)

    hasGuards = any (not . null . guardsOf . unLoc) grhssGRHSs

    rhsSpan = foldr1 (<>) (fmap (grhsSpan . unLoc) grhssGRHSs)
    bodySpan = fmap endOf endOfPats <> rhsSpan

    placement = case endOfPats of
      Just spn
        | any (longGuard . unLoc) grhssGRHSs || not (sameLine (Just spn) rhsSpan) ->
            Normal
      _ -> blockPlacement (bodyIn mkBody bracing) grhssGRHSs
    longGuard grhs = case guardsOf grhs of
      [] -> False
      [g] -> not (maybe True isSingleLine (spanOf g))
      _ -> True

    separator
      | length grhssGRHSs > 1 = mempty
      | otherwise = case style of
          FunctionStyle _ | hasGuards -> mempty
          FunctionStyle _ -> space <> indent (txt "=")
          PatternBindStyle | hasGuards -> mempty
          PatternBindStyle -> space <> indent (txt "=")
          s | isCaseStyle s && hasGuards -> mempty
          _ -> space <> txt "->"

    body = sepBy breakOrSpace (map alternative (NE.toList grhssGRHSs))

    alternative g =
      fenceWithin ctx (spanOf g) $
        atSpan
          ctx
          (grhsSpan (unLoc g))
          (guardedRhs ctx placement bracing mkBody groupStyle (unLoc g))

    groupStyle
      | isCaseStyle style && hasGuards = RightArrow
      | otherwise = EqualsSign

    whereClause = case grhssLocalBinds of
      EmptyLocalBinds _ -> mempty
      binds ->
        breakOrSpace
          <> keywordAt ctx (whereKeywordSpan binds) "where"
          <> includeUnless
            (isEmptyLocalBinds binds)
            (breakOrSpace <> indent (localBinds ctx bracing binds))

isCaseStyle :: MatchStyle -> Bool
isCaseStyle = \case
  CaseStyle -> True
  LambdaCaseStyle -> True
  _ -> False

containsOrPat :: LPat GhcPs -> Bool
containsOrPat = any isOrPat . listify (const True :: Pat GhcPs -> Bool)
  where
    isOrPat = \case
      OrPat {} -> True
      _ -> False

grhsSpan :: GRHS GhcPs (LocatedA body) -> Maybe Span
grhsSpan (GRHS _ guards body) = spanOf body <> spansOf guards

-- | The guards of an alternative.
guardsOf :: GRHS GhcPs body -> [GuardLStmt GhcPs]
guardsOf (GRHS _ guards _) = guards

-- | The placement of a body that is the whole of an equation.
blockPlacement ::
  (Body b) =>
  (LocatedA body -> b) ->
  NonEmpty (LGRHS GhcPs (LocatedA body)) ->
  Placement
blockPlacement bodyOf = \case
  L _ (GRHS _ _ body) :| [] -> bodyPlacement (bodyOf body)
  _ -> Normal

-- | One alternative of an equation: its guards, and what they guard.
guardedRhs ::
  (Body b) =>
  Ctx ->
  -- | How the equation as a whole is placed.
  Placement ->
  -- | Bracing the body inherits.
  Bracing ->
  BodyOf body b ->
  GuardStyle ->
  GRHS GhcPs (LocatedA body) ->
  Doc
guardedRhs ctx parentPlacement bracing mkBody style (GRHS _ guards body) = case guards of
  [] -> printBody bound
  _ ->
    txt "|"
      <> space
      <> align (commaSep (map (align . hsStmt ctx) guards))
      <> space
      <> indent (txt separator)
      <> nest
        (if parentPlacement == Normal then 1 else 0)
        (attach placement (printBody bound))
  where
    bound = bodyIn mkBody bracing body
    separator = case style of
      EqualsSign -> "="
      RightArrow -> "->"
    placement
      | maybe True (\g -> sameLine (Just g) (spanOf body)) endOfGuards =
          bodyPlacement bound
      | otherwise = Normal
    endOfGuards = case guards of
      [] -> Nothing
      _ -> spanOf (last guards)

-- | A pattern synonym binding.
patSynBind :: Ctx -> PatSynBind GhcPs GhcPs -> Doc
patSynBind ctx PSB {..} =
  txt "pattern" <> case psb_args of
    PrefixCon args ->
      space
        <> name ctx psb_id
        <> indent
          ( layoutAcross ctx args (argsAfterName (map (name ctx) args) (null args))
              <> definition (spansOf args)
          )
    RecCon args ->
      space
        <> name ctx psb_id
        <> indent
          ( layoutAcross
              ctx
              (vars args)
              ( includeUnless (null args) breakOrSpace
                  <> braces (commaSep (map (name ctx) (vars args)))
              )
              <> definition (spansOf (vars args))
          )
      where
        vars = map recordPatSynPatVar
    InfixCon l r ->
      layoutFrom
        ctx
        (spanOf l <> spanOf r)
        (space <> name ctx l <> breakOrSpace <> indent (name ctx psb_id <> space <> name ctx r))
        <> indent (definition (spanOf l <> spanOf r))
  where
    argsAfterName rendered isEmpty =
      includeUnless isEmpty breakOrSpace <> align (sepBy breakOrSpace rendered)

    definition argSpans =
      space <> case psb_dir of
        Unidirectional -> rhs "<-"
        ImplicitBidirectional -> rhs "="
        ExplicitBidirectional mg ->
          rhs "<-"
            <> breakOrSpace
            <> txt "where"
            <> breakOrSpace
            <> indent (matchGroup ctx NoBrace (ExprBody ctx) (FunctionStyle psb_id) mg)
      where
        rhs arrow =
          layoutFrom ctx (spanOf psb_id <> spanOf psb_def <> argSpans) $
            txt arrow <> breakOrSpace <> hsPat ctx psb_def

-- | An untyped splice, either @$x@ or a quasi-quotation.
untypedSplice :: Ctx -> SpliceDecoration -> HsUntypedSplice GhcPs -> Doc
untypedSplice ctx deco = \case
  HsUntypedSpliceExpr _ e -> spliceTH ctx False e deco
  HsQuasiQuote _ quoter str ->
    txt "["
      <> name ctx quoter
      <> txt "|"
      <> at ctx str (verbatim . T.pack . unpackFS)
      <> txt "|]"

spliceTH :: Ctx -> Bool -> LHsExpr GhcPs -> SpliceDecoration -> Doc
spliceTH ctx isTyped e = \case
  DollarSplice -> txt (if isTyped then "$$" else "$") <> spliced
  BareSplice -> spliced
  where
    spliced = at ctx e (align . exprBody ctx plainSite (spanOf e))

-- | A Template Haskell quotation.
quotation :: Ctx -> HsQuote GhcPs -> Doc
quotation ctx = \case
  ExpBr (bracketAnn, _) e -> quoted (flavour bracketAnn) (hsExpr ctx e)
    where
      flavour = \case
        BracketNoE {} -> ""
        BracketHasE {} -> "e"
  PatBr _ p -> quoted "p" (hsPat ctx p)
  DecBrL _ decls ->
    quoted "d" (starGuard decls (knotDecls (ctxKnot ctx) ctx Free decls))
  DecBrG _ _ -> error "Tilia: DecBrG is produced by the renamer"
  TypBr _ ty -> quoted "t" (starGuard ty (hsType ctx ty))
  VarBr _ isSingle n -> txt (if isSingle then "'" else "''") <> name ctx n
  where
    quoted flavour body =
      txt "["
        <> txt flavour
        <> txt "|"
        <> breakOrNothing
        <> indent (body <> breakOrNothing <> txt "|]")
    starGuard x body
      | risky = space <> body <> space
      | otherwise = body
      where
        risky =
          any isStar (listify (const True :: HsType GhcPs -> Bool) x)
            || any isAbstract (listify (const True :: FamilyInfo GhcPs -> Bool) x)
    isStar = \case
      HsStarTy {} -> True
      _ -> False
    isAbstract = \case
      ClosedTypeFamily Nothing -> True
      _ -> False

-- | Let a block brace itself when the surrounding layout is flat.
whenFlat :: Bracing -> (Bracing -> Doc) -> Doc
whenFlat whenBroken render = variant (render MayBrace) (render whenBroken)

-- | A @case@ or lambda standing as a block item has to delimit itself when
-- the block is flat, unless it is the function of an application, where the
-- argument that follows already does so.
bracedForSite :: Site -> (Bracing -> Doc) -> Doc
bracedForSite site render
  | siteInBlock site && not (siteApplicand site) =
      whenFlat (siteBracing site) render
  | otherwise = render (siteBracing site)
