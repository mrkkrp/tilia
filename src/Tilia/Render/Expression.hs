{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Expressions, and the equations and blocks built out of them.
module Tilia.Render.Expression
  ( hsExpr,
    hsExprIn,
    valDecl,
    untypedSplice,
  )
where

import Control.Applicative ((<|>))
import Data.Char (isSpace)
import Data.Choice (Choice, fromBool, isTrue, pattern Is, pattern Isn't)
import Data.Data (Data)
import Data.Generics.Schemes (listify)
import Data.List (unsnoc)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (unpackFS)
import GHC.Hs hiding (Fixity, UnicodeSyntax)
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.Basic (Boxity (..), funPrec)
import GHC.Types.Fixity (LexicalFixity (..))
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..), mkVarUnqual, rdrNameOcc)
import GHC.Types.SrcLoc
  ( GenLocated (..),
    getLoc,
    isZeroWidthSpan,
    noSrcSpan,
    unLoc,
  )
import Language.Haskell.Syntax.Basic (field_label)

import Tilia.Doc.Combinators
import Tilia.Fixity
  ( Fixity,
    Namespace (..),
  )
import Tilia.Render.Context
import Tilia.Render.Layout
import Tilia.Render.Literal (hsLit, writtenString)
import Tilia.Render.Name
import Tilia.Render.Operator
import Tilia.Render.Pattern
import Tilia.Render.Type
import Tilia.Settings (indentationColumns)
import Tilia.Source.Lines (contentColumn)
import Tilia.Span
import Tilia.Span.Ghc

-- | An expression.
hsExpr :: Ctx -> LHsExpr GhcPs -> Doc
hsExpr ctx = hsExprIn ctx plainSite

-- | An expression that knows where it stands.
hsExprIn :: Ctx -> Site -> LHsExpr GhcPs -> Doc
hsExprIn ctx site l = at ctx l (exprBody ctx site (spanOf l))

-- | Every shape an expression can take.
exprBody ::
  -- | The context.
  Ctx ->
  -- | Where it stands.
  Site ->
  -- | Where it was.
  Maybe Span ->
  -- | The expression.
  HsExpr GhcPs ->
  Doc
exprBody ctx site here = \case
  HsVar _ n -> name ctx n
  HsOverLabel src _ -> txt "#" <> sourceText src
  HsIPVar _ (HsIPName n) -> txt "?" <> outputable n
  HsOverLit _ v -> outputable (ol_val v)
  HsLit _ lit -> hsLit lit
  HsLam _ variant' mg -> lambda ctx site variant' (ExprBody ctx) mg
  HsApp _ f x -> application ctx site f x
  HsAppType at' e a ->
    hsExprIn ctx (leftmost site) e
      >+> breakOrSpace
        <> atSpan
          ctx
          (spanOf at' <> spanOf (hswc_body a))
          (keywordAt (spanOf at') "@" <> hsType ctx (hswc_body a))
  OpApp _ x op y -> exprChain ctx site x op y
  NegApp minus e _ ->
    keywordAt (spanOf minus) "-"
      >+> includeWhen (isLiteral e) (negativeLiteralGap ctx)
        <> includeWhen (startsWithSymbol e) space
        <> hsExpr ctx e
  HsPar tokens e ->
    layoutWithin ctx here (spanOf e) $
      bracketWith
        (closingFor site)
        (pairSpans tokens)
        "("
        ")"
        (insideBrackets here (apart (hsExpr ctx e)))
    where
      apart d = case unLoc e of
        SectionR _ (L _ (HsVar _ (L _ (Unqual o)))) _
          | unboxedParens, take 1 (occNameString o) == "#" -> space <> d
        SectionL _ _ (L _ (HsVar _ (L _ (Unqual o))))
          | unboxedParens, occNameString o == "#" -> d <> space
        _ -> d
      unboxedParens =
        extensionOn ctx UnboxedTuples || extensionOn ctx UnboxedSums
  SectionL _ x op -> hsExpr ctx x >+> breakOrSpace <> hsExpr ctx op
  SectionR _ op x -> hsExpr ctx op >+> breakOrSpace <> hsExpr ctx x
  ExplicitTuple brackets args boxity ->
    tuple ctx here (closingFor site) (pairSpans brackets) boxity args
  ExplicitSum ann tag arity e ->
    unboxedSum
      (closingFor site)
      (spanOf (aesOpen ann), spanOf (aesClose ann))
      tag
      arity
      (hsExpr ctx e)
  HsCase anns e mg -> caseOf ctx site (ExprBody ctx) anns e mg
  HsIf anns c t e ->
    ifThenElse ctx (bodyIn (ExprBody ctx) (siteBracing site)) anns c t e
  HsMultiIf (ifToken, _, _) guards ->
    indent
      (underSite site)
      (refAt (spanOf ifToken) (txt "if"))
      (breakOrSpace <> sepBy breakOrSpace (fmap alternative (NE.toList guards)))
    where
      alternative =
        guardedRhs ctx Normal (siteBracing site) (ExprBody ctx) RightArrow
  HsLet tokens binds e ->
    letIn ctx (bodyIn (ExprBody ctx) (siteBracing site)) nested tokens binds e
    where
      nested = case unLoc e of
        HsLet{} -> True
        _ -> False
  HsDo anns flavour es -> case flavour of
    DoExpr moduleName -> doBlock moduleName "do"
    MDoExpr moduleName -> doBlock moduleName "mdo"
    ListComp -> comprehension ctx site here (bracketSpans (al_brackets anns)) es
    MonadComp ->
      comprehension ctx site here (bracketSpans (al_brackets anns)) es
    GhciStmtCtxt -> error "Tilia: GhciStmtCtxt cannot occur in a source file"
    where
      doBlock moduleName word =
        statements
          ctx
          site
          (ExprBody ctx)
          ( foldMap (\m -> outputable m <> txt ".") moduleName
              <> keywordAt (doKeywordSpan anns) word
          )
          es
  ExplicitList ann xs ->
    commaBracketWith
      (ctxCommaStyle ctx)
      (closingFor site)
      (bracketSpans (al_brackets ann))
      "["
      "]"
      ( insideBrackets
          here
          (commaSep (ctxCommaStyle ctx) (fmap (align . item . hsExpr ctx) xs))
      )
    where
      item
        | maybe True (writtenOnOneLine ctx) here = id
        | otherwise = infrangible
  RecordCon{..} ->
    name ctx rcon_con
      >+> breakOrNothing
        <> bracketWith
          0
          (pairSpans rcon_ext)
          "{"
          "}"
          ( insideBrackets
              here
              (commaSep (ctxCommaStyle ctx) (fmap align (fields <> wildcard)))
          )
    where
      HsRecFields{..} = rcon_flds
      fields =
        fmap
          (at_ ctx (fieldBind ctx (at_ ctx (name ctx . foLabel))))
          rec_flds
      wildcard = case rec_dotdot of
        Just l -> [at ctx l (const (txt ".."))]
        Nothing -> []
  RecordUpd{..} ->
    hsExprIn ctx (leftmost site) rupd_expr
      >+> breakOrNothing
        <> bracketWith
          0
          (pairSpans rupd_ext)
          "{"
          "}"
          (insideBrackets here updates)
    where
      updates = case rupd_flds of
        RegularRecUpdFields{..} ->
          commaSep
            (ctxCommaStyle ctx)
            ( fmap
                (align . at_ ctx (fieldBind ctx (at_ ctx (fieldOcc ctx))))
                recUpdFields
            )
        OverloadedRecUpdFields{..} ->
          commaSep
            (ctxCommaStyle ctx)
            ( fmap
                (align . at_ ctx (fieldBind ctx (at_ ctx labelChain)))
                olRecUpdFields
            )
      labelChain (FieldLabelStrings flss) = dotFields ctx (unLoc <$> flss)
  HsGetField{..} ->
    hsExprIn ctx (leftmost site) gf_expr <> at ctx gf_field (dotted ctx)
  HsProjection{..} ->
    parens
      (spanOf (apOpen proj_ext), spanOf (apClose proj_ext))
      (foldMap (dotted ctx) proj_flds)
  ExprWithTySig _ x HsWC{hswc_body} ->
    align $
      if endsWithBlock (unLoc x)
        then
          subject
            <> breakOrSpace
            <> (txt (reserved ctx "::") >+> space <> hsSigType ctx hswc_body)
        else subject >+> typeAscription ctx (spanOf x) hswc_body
    where
      subject = hsExprIn ctx (withBracing MayBrace (leftmost site)) x
  ArithSeq ann _ range ->
    arithSeq
      ctx
      here
      (closingFor site)
      (spanOf (aas_open ann), spanOf (aas_close ann))
      range
  HsTypedBracket (bracketAnn, close) e ->
    quoted here (open, spanOf close) opener "||]" e (hsExpr ctx e)
    where
      (open, opener) = case bracketAnn of
        BracketNoE t -> (spanOf t, "[||")
        BracketHasE t -> (spanOf t, "[e||")
  HsUntypedBracket _ q -> quotation ctx here q
  HsTypedSplice _ (HsTypedSpliceExpr tok e) ->
    spliceTH ctx (Is #typed) (spanOf tok) e DollarSplice
  HsUntypedSplice _ splice -> untypedSplice ctx DollarSplice splice
  HsProc _ p e ->
    ( txt "proc"
        >+> layoutFrom ctx (spanOf p) (breakOrSpace <> infrangible (hsPat ctx p))
          <> space
          <> txt (reserved ctx "->")
    )
      >+> attachBody
        (CmdTopBody ctx (withBracing (siteBracing site) plainSite) e)
  HsStatic _ e ->
    txt "static"
      >+> breakOrSpace
        <> hsExprIn ctx (withBracing (siteBracing site) plainSite) e
  HsPragE _ prag x -> case prag of
    HsPragSCC _ n ->
      indent
        (if siteInBlock site then 1 else 0)
        (txt "{-# SCC " <> writtenString n <> txt " #-}")
        ( breakOrSpace
            <> hsExprIn ctx (withBracing (siteBracing site) plainSite) x
        )
  HsEmbTy _ HsWC{hswc_body} -> txt "type" <> space <> hsType ctx hswc_body
  HsHole holeKind -> case holeKind of
    HoleVar n -> name ctx n
    HoleError ->
      error "Tilia: a nameless hole cannot come from a successful parse"
  HsForAll _ tele e ->
    forallTelescope ctx breakOrSpace (spanOf e) tele <> hsExpr ctx e
  HsQual _ qs e ->
    at
      ctx
      qs
      ( contextOf
          ctx
          (spanOf qs)
          (contextParens qs)
          bare
          (hsExpr ctx)
          . fmap unbracketed
      )
      <> joinedByArrow
        ctx
        breakOrSpace
        (spanOf qs)
        (spanOf e)
        (reserved ctx "=>")
      <> hsExpr ctx e
    where
      -- funPrec is opPrec, yet a context may be an operator application.
      bare c = case unLoc c of
        OpApp{} -> True
        other -> not (hsExprNeedsParens funPrec other)
  HsFunArr _ multAnn x y ->
    hsExpr ctx x
      <> joinedByFunctionArrow
        ctx
        (hsExpr ctx)
        breakOrSpace
        (spanOf x)
        (spanOf y)
        multAnn
      <> case unLoc y of
        HsFunArr{} -> exprBody ctx plainSite (spanOf y) (unLoc y)
        _ -> hsExpr ctx y

-- | The body of an equation, as the enclosing construct will hand it on.
type BodyOf body b = Site -> LocatedA body -> b

-- | A body standing on its own, with the given bracing.
bodyIn :: BodyOf body b -> Bracing -> LocatedA body -> b
bodyIn mkBody bracing = mkBody (withBracing bracing plainSite)

-- | Is this expression a literal?
isLiteral :: LHsExpr GhcPs -> Bool
isLiteral e = case unLoc e of
  HsLit{} -> True
  HsOverLit{} -> True
  _ -> False

-- | Does this expression begin with a symbol character, which an operator
-- written right before it would take into its name?
startsWithSymbol :: LHsExpr GhcPs -> Bool
startsWithSymbol e = case unLoc e of
  HsIPVar{} -> True
  HsOverLabel{} -> True
  HsLam{} -> True
  HsUntypedSplice _ HsUntypedSpliceExpr{} -> True
  HsTypedSplice{} -> True
  HsApp _ f _ -> startsWithSymbol f
  HsAppType _ f _ -> startsWithSymbol f
  RecordUpd{rupd_expr} -> startsWithSymbol rupd_expr
  HsGetField{gf_expr} -> startsWithSymbol gf_expr
  _ -> False

-- | A function applied to arguments.
application :: Ctx -> Site -> LHsExpr GhcPs -> LHsExpr GhcPs -> Doc
application ctx site f x =
  case placement of
    Normal ->
      mayBraceWhenFlat (siteBracing site) headAndInit
        <> indent 1 headPtr (breakOrSpace <> lastArgDoc)
    Hanging ->
      infrangible (layoutFrom ctx initSpan (headAndInit MayBrace))
        <> indent 1 headPtr (attach Hanging lastArgDoc)
  where
    (func, args) = gatherArgs f (x :| [])
    initArgs = NE.init args
    lastArg = NE.last args
    lastArgDoc =
      hsExprIn
        ctx
        plainSite
          { siteFollowed = siteFollowed site,
            siteBracing = siteBracing site
          }
        lastArg
    initSpan = spanOf f <> (startOf <$> spanOf lastArg)
    placement
      | maybe False (writtenOnOneLine ctx) initSpan =
          exprHangs ctx (unLoc lastArg)
      | otherwise = Normal
    funcDoc bracing =
      hsExprIn ctx site{siteFollowed = True, siteBracing = bracing} func
    headPtr = ptr (funcDoc MayBrace)
    headAndInit bracing =
      ref (funcDoc bracing)
        <> foldMap
          ( indent (if placement == Hanging then 0 else 1) headPtr
              . (breakOrSpace <>)
              . hsExprIn ctx (withBracing bracing plainSite)
          )
          initArgs

-- | The head of an application, and its arguments in the order written.
gatherArgs ::
  LHsExpr GhcPs ->
  NonEmpty (LHsExpr GhcPs) ->
  (LHsExpr GhcPs, NonEmpty (LHsExpr GhcPs))
gatherArgs f known = case unLoc f of
  HsApp _ l r -> gatherArgs l (NE.cons r known)
  _ -> (f, known)

-- | A tuple, or a tuple section.
tuple ::
  -- | The context.
  Ctx ->
  -- | Where it was written.
  Maybe Span ->
  -- | How many steps in from the opening bracket the closing one goes.
  Int ->
  -- | Where the brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | Whether it is boxed.
  Boxity ->
  -- | What it holds.
  [HsTupArg GhcPs] ->
  Doc
tuple ctx here closing brackets boxity args
  | any isMissing args = flat (brackets' (sepBy comma (fmap arg args)))
  | otherwise = brackets' (commaSep (ctxCommaStyle ctx) (fmap arg args))
  where
    brackets' =
      boxityBrackets (ctxCommaStyle ctx) boxity closing brackets
        . insideBrackets here
    isMissing = \case
      Missing _ -> True
      _ -> False
    arg =
      align . \case
        Present _ e -> hsExpr ctx e
        Missing _ -> mempty

-- | An arithmetic sequence, in whichever of its four forms.
arithSeq ::
  -- | The context.
  Ctx ->
  -- | Where it was written.
  Maybe Span ->
  -- | How many steps in from the opening bracket the closing one goes.
  Int ->
  -- | Where the brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | The sequence.
  ArithSeqInfo GhcPs ->
  Doc
arithSeq ctx here closing brackets = \case
  From from -> wrap (hsExpr ctx from <> breakOrSpace <> txt "..")
  FromThen from next ->
    wrap
      ( commaSep (ctxCommaStyle ctx) (fmap (hsExpr ctx) [from, next])
          <> breakOrSpace
          <> txt ".."
      )
  FromTo from to ->
    wrap (hsExpr ctx from <> breakOrSpace <> txt ".." <> space <> hsExpr ctx to)
  FromThenTo from next to ->
    wrap $
      commaSep (ctxCommaStyle ctx) (fmap (hsExpr ctx) [from, next])
        <> breakOrSpace
        <> txt ".."
        <> space
        <> hsExpr ctx to
  where
    wrap =
      commaBracketWith (ctxCommaStyle ctx) closing brackets "[" "]"
        . insideBrackets here

-- | One field of a record construction or update.
fieldBind ::
  HasLoc l =>
  Ctx ->
  (GenLocated l a -> Doc) ->
  HsFieldBind (GenLocated l a) (LHsExpr GhcPs) ->
  Doc
fieldBind ctx label HsFieldBind{..}
  | hfbPun = label hfbLHS
  | otherwise =
      label hfbLHS
        >+> trailing (keywordAt (spanOf =<< hfbAnn) "=")
          <> attach placement (hsExpr ctx hfbRHS)
  where
    placement
      | sameLine (spanOf hfbLHS) (spanOf hfbRHS) = exprHangs ctx (unLoc hfbRHS)
      | otherwise = Normal

-- | One field name written after a dot.
dotField :: Ctx -> DotFieldOcc GhcPs -> Doc
dotField ctx = name ctx . fmap (mkVarUnqual . field_label) . dfoLabel

-- | A run of field names, joined by the dots between them.
dotFields ::
  -- | The context.
  Ctx ->
  -- | The field names.
  NonEmpty (DotFieldOcc GhcPs) ->
  Doc
dotFields ctx (f :| fs) = dotField ctx f <> foldMap (dotted ctx) fs

-- | One field name with the dot written right before it.
dotted ::
  -- | The context.
  Ctx ->
  -- | The field.
  DotFieldOcc GhcPs ->
  Doc
dotted ctx f =
  keywordAt (spanOf =<< afDot (dfoExt f)) "." <> dotField ctx f

-- | A chain of operators applied to expressions.
exprChain ::
  Ctx ->
  Site ->
  LHsExpr GhcPs ->
  LHsExpr GhcPs ->
  LHsExpr GhcPs ->
  Doc
exprChain ctx site x op y =
  operatorChain
    ctx
    site
    (ExprBody ctx)
    isDoBlock
    (flattenAround splitOpApp x op y)

-- | Take an operator application apart, if that is what this is.
splitOpApp ::
  LHsExpr GhcPs ->
  Maybe (LHsExpr GhcPs, LHsExpr GhcPs, LHsExpr GhcPs)
splitOpApp e = case unLoc e of
  OpApp _ l o r -> Just (l, o, r)
  _ -> Nothing

-- | The fixity of an operator used in an expression.
fixityOf :: Ctx -> LHsExpr GhcPs -> Maybe Fixity
fixityOf ctx o =
  (spanOf o >>= (`Map.lookup` ctxCaptured ctx))
    <|> (operatorName o >>= operatorFixity ctx InTerms)

-- | Regroup a run of operators applied to expressions or commands by
-- precedence, and print it.
operatorChain ::
  (Body b, HasLoc a) =>
  -- | The context.
  Ctx ->
  -- | Where the chain stands.
  Site ->
  -- | An operand, as a body.
  (Site -> a -> b) ->
  -- | Is this operand a @do@ block?
  (a -> Bool) ->
  -- | The operands, and the operators between them.
  (NonEmpty a, [LHsExpr GhcPs]) ->
  Doc
operatorChain ctx outerSite mkBody isDo =
  render False outerSite . uncurry (associate (fixityOf ctx))
  where
    hangs = bodyPlacement . mkBody plainSite
    render afterOperator site = \case
      Operand e -> printBody (mkBody site e)
      chain@(Chain operands@(firstOne :| _) operators) ->
        layoutFrom ctx (chainSpan spanOf chain) $
          if operatorsTrail
            then laidOut MayBrace
            else
              mayBraceWhenFlat
                (if placement == Hanging then MayBrace else NoBrace)
                laidOut
        where
          placement
            | leadingDo && not (siteInBlock site) = Normal
            | otherwise =
                placementOf (chainSpan spanOf firstOne) (NE.last operands)
          placementOf leader operand
            | ledByDo operand = Normal
            | otherwise = chainPlacement hangs leader operand
          ledByDo = \case
            Chain (Operand e :| _) _ -> doWithinAStep ctx isDo e
            Chain (first :| _) _ -> ledByDo first
            Operand _ -> False
          laidOut bracing =
            let first' =
                  infrangible
                    ( render
                        afterOperator
                        ( followedBy
                            (listToMaybe operators)
                            (withBracing (firstBracing bracing) site)
                        )
                        firstOne
                    )
                fromFirst =
                  indent
                    (if leadingDo && not (siteInBlock site) then 0 else 1)
                    (ptr first')
                pieces = links operands operators
             in ref first'
                  <> if operatorsTrail
                    then
                      fromFirst (foldr (piece bracing fromFirst) mempty pieces)
                    else foldr (piece bracing fromFirst) mempty pieces
          followedBy next s =
            s{siteFollowed = maybe (siteFollowed site) isMinus next}
          -- A step would carry the operators on the last statement of a do
          -- block written on one line before them, unless an operator puts
          -- the block further right. They stay at the chain's indentation
          -- instead, as Fourmolu has them, but in a block, where that would
          -- begin a statement, the do block is braced.
          leadingDo = case firstOne of
            Operand e -> not afterOperator && doWithinAStep ctx isDo e
            Chain{} -> False
          firstBracing bracing
            | leadingDo && siteInBlock site = MayBrace
            | otherwise = bracing
          operatorLine placing previous o operand rendered
            | leadingDo && not (siteInBlock site) =
                breakOrSpace <> attachedWith Hanging
            | otherwise = attachedWith placement
            where
              attachedWith p =
                attachOperator
                  ctx
                  p
                  placing
                  (chainSpan spanOf previous)
                  (spanOf o)
                  (chainSpan spanOf operand)
                  (hsExpr ctx o)
                  rendered
          piece bracing placing (previous, o, operand, next) rest' =
            let isLast = isNothing next
                operandSite =
                  followedBy next $
                    withBracing
                      (if isLast then siteBracing site else bracing)
                      plainSite
                kept
                  | isLast = id
                  | otherwise = infrangible
                rendered =
                  kept (render (not operatorsTrail) operandSite operand)
             in if operatorsTrail
                  then
                    trailing (hsExpr ctx o)
                      >+> attach
                        ( tailPlacement
                            isLast
                            (chainSpan spanOf previous)
                            o
                            operand
                        )
                        (if isLast then rendered else rendered >+> rest')
                  else operatorLine placing previous o operand rendered <> rest'
          tailPlacement isLast before o operand
            | isLast,
              not (maybe True isSingleLine (chainSpan spanOf operand)),
              not (lineCommentWrittenBetween ctx before (spanOf o)) =
                placementOf (spanOf o) operand
            | otherwise = Normal
          commentedOperators =
            or (zipWith commentedBefore (NE.toList operands) operators)
          commentedBefore operand o =
            commentPrintedBetween ctx (chainSpan spanOf operand) (spanOf o)
          operatorsTrail =
            (length operators == 1 || endsHanging)
              && not commentedOperators
              && and (zipWith couldTrail (NE.toList operands) operators)
          endsHanging = hangs (lastOperand chain) == Hanging
          couldTrail previous o =
            isSeparator (fixityOf ctx o)
              && maybe False (writtenOnOneLine ctx) (chainSpan spanOf previous)
              && placement == Normal
              && not (isDo (lastOperand previous))

-- | Is this a @do@ or @mdo@ block written on one line, with its statements
-- nearer to where it begins than a step?
doWithinAStep ::
  HasLoc a =>
  -- | The context.
  Ctx ->
  -- | Is this a @do@ block?
  (a -> Bool) ->
  -- | The expression or command.
  a ->
  Bool
doWithinAStep ctx isDo e =
  indentationColumns (ctxIndentation ctx) > 2
    && isDo e
    && maybe False (writtenOnOneLine ctx) (spanOf e)

-- | Is this expression a @do@ or @mdo@ block?
isDoBlock :: LHsExpr GhcPs -> Bool
isDoBlock e = case unLoc e of
  HsDo _ (DoExpr _) _ -> True
  HsDo _ (MDoExpr _) _ -> True
  _ -> False

-- | Whether an operand hangs, which it can only where it starts on the line
-- the given span starts on.
chainPlacement ::
  HasLoc a =>
  (a -> Placement) ->
  Maybe Span ->
  OpChain a op ->
  Placement
chainPlacement placer leader operand = case operand of
  Operand x | startsTogether -> placer x
  Chain (first :| _) _ -> chainPlacement placer leader first
  _ -> Normal
  where
    startsTogether =
      case (leader, chainSpan spanOf operand) of
        (Just a, Just b) -> spanStartLine a == spanStartLine b
        _ -> False

-- | What is inside, however many brackets were written around it.
unbracketed :: LHsExpr GhcPs -> LHsExpr GhcPs
unbracketed e = case unLoc e of
  HsPar _ inner -> unbracketed inner
  _ -> e

-- | A statement of a comprehension or a guard.
hsStmt :: Ctx -> ExprLStmt GhcPs -> Doc
hsStmt ctx = at_ ctx (stmtBody ctx plainSite (ExprBody ctx))

-- | One statement of a block, a comprehension or a guard.
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
  BindStmt arrow p f ->
    hsPatClosing ctx (closingFor site) p
      >+> trailing (keywordAt (spanOf arrow) (reserved ctx "<-"))
        <> layoutFrom
          ctx
          (fmap endOf (spanOf p) <> spanOf f)
          (attach placement (printBody bound))
    where
      bound = bodyIn mkBody (siteBracing site) f
      placement
        | sameLine (spanOf p) (spanOf f) = bodyPlacement bound
        | otherwise = Normal
  LetStmt _ binds ->
    txt "let" <> space <> align (localBinds ctx (siteBracing site) binds)
  ParStmt{} ->
    error "Tilia: ParStmt should have been unpacked"
  TransStmt{..} -> layoutFrom ctx own $ case (trS_form, trS_by) of
    (ThenForm, Nothing) ->
      txt "then" >+> breakOrSpace <> hsExpr ctx trS_using
    (ThenForm, Just e) ->
      (txt "then" >+> breakOrSpace <> hsExpr ctx trS_using)
        <> breakOrSpace
        <> (txt "by" >+> breakOrSpace <> hsExpr ctx e)
    (GroupForm, Nothing) ->
      txt "then group using" >+> breakOrSpace <> hsExpr ctx trS_using
    (GroupForm, Just e) ->
      (txt "then group by" >+> breakOrSpace <> hsExpr ctx e)
        <> breakOrSpace
        <> (txt "using" >+> breakOrSpace <> hsExpr ctx trS_using)
    where
      own =
        spanOf (ats_then trS_ext) <> spanOf trS_using <> foldMap spanOf trS_by
  RecStmt{..} ->
    txt "rec" <> space <> align (stmtBlock ctx site mkBody recS_stmts)

-- | The statements of a block after the keyword that opens it, with the
-- break that introduces them.
statements ::
  ( Body b,
    Anno [LStmt GhcPs (XRec GhcPs body)] ~ SrcSpanAnnLW,
    Anno (Stmt GhcPs (XRec GhcPs body)) ~ SrcSpanAnnA,
    Anno body ~ SrcSpanAnnA
  ) =>
  -- | The context.
  Ctx ->
  -- | Where the block stands.
  Site ->
  -- | Each body, as the block hands it on.
  BodyOf body b ->
  -- | The keyword.
  Doc ->
  -- | The statements.
  XRec GhcPs [LStmt GhcPs (XRec GhcPs body)] ->
  Doc
statements ctx site mkBody keyword es =
  indent
    (underSite site)
    keyword
    (breakOrSpace <> stmtBlock ctx site mkBody es)

-- | The statements of a block, each laid out as an item of it.
stmtBlock ::
  ( Body b,
    Anno [LStmt GhcPs (XRec GhcPs body)] ~ SrcSpanAnnLW,
    Anno (Stmt GhcPs (XRec GhcPs body)) ~ SrcSpanAnnA,
    Anno body ~ SrcSpanAnnA
  ) =>
  -- | The context.
  Ctx ->
  -- | Where the block stands.
  Site ->
  -- | Each body, as the block hands it on.
  BodyOf body b ->
  -- | The statements.
  XRec GhcPs [LStmt GhcPs (XRec GhcPs body)] ->
  Doc
stmtBlock ctx site mkBody es = maybe id located (spanOf es) (block (unLoc es))
  where
    block xs =
      itemsOrBraces (getLoc es) (siteBracing site) (separatedByBlank ctx) $
        [ (spanOf stmt, item position stmt)
        | (position, stmt) <- positioned xs
        ]
    item position stmt =
      bracedByPosition position NoBrace $ \bracing ->
        at ctx stmt (stmtBody ctx (blockSite bracing) mkBody)
    blockSite bracing =
      plainSite{siteInBlock = True, siteBracing = bracing}

-- | A list comprehension.
comprehension ::
  -- | The context.
  Ctx ->
  -- | Where it stands.
  Site ->
  -- | Where it was written.
  Maybe Span ->
  -- | Where its brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | What it yields, and the statements it yields it from.
  XRec GhcPs [ExprLStmt GhcPs] ->
  Doc
comprehension ctx site here (o, c) es = align (variant onOneLine acrossLines)
  where
    onOneLine = opening <> body <> closing
    acrossLines = opening <> space <> keepInside (body <> hardBreak <> closing)
    opening = openingAt o "["
    closing = closingAt c "]"
    keepInside = if siteInBlock site then align else id
    body = insideBrackets here (at ctx es sections)
    sections xs = case unsnoc xs of
      Nothing -> error "Tilia: a comprehension always yields something"
      Just (stmts, yield) ->
        align (hsStmt ctx yield)
          <> breakOrSpace
          <> sepBy breakOrSpace (fmap section (comprehensionSections stmts))
    section stmts =
      located'
        (spansOf stmts)
        ( commaSepAfter
            (ctxCommaStyle ctx)
            "|"
            (fmap (align . hsStmt ctx) stmts)
        )
    located' = maybe id located

-- | Split the statements of a comprehension into its parallel sections.
comprehensionSections :: [ExprLStmt GhcPs] -> [[ExprLStmt GhcPs]]
comprehensionSections = fmap unnest . branches
  where
    branches = \case
      [L _ (ParStmt _ blocks _ _)] ->
        [run | ParStmtBlock _ run _ _ <- NE.toList blocks]
      run -> [run]
    unnest = concatMap $ \case
      L _ ParStmt{} -> error "Tilia: parallel blocks do not nest"
      stmt@(L _ TransStmt{trS_stmts}) -> unnest trS_stmts <> [stmt]
      stmt -> [stmt]

-- | A @case@ expression or command.
caseOf ::
  ( Body b,
    Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
    Anno [LMatch GhcPs (LocatedA body)] ~ SrcSpanAnnLW,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  -- | The context.
  Ctx ->
  -- | Where it stands.
  Site ->
  -- | Each body, as the alternatives hand it on.
  BodyOf body b ->
  -- | Where its keywords were.
  EpAnnHsCase ->
  -- | The scrutinee.
  LHsExpr GhcPs ->
  -- | The alternatives.
  MatchGroup GhcPs (LocatedA body) ->
  Doc
caseOf ctx site mkBody EpAnnHsCase{..} scrutinee mg =
  ref keyword
    <> indent
      1
      (ptr keyword)
      ( space
          <> hsExpr ctx scrutinee
          <> trailing (keywordAt (spanOf hsCaseAnnOf) "of")
      )
    <> indent
      (underSite site)
      (ptr keyword)
      (breakOrSpace <> matchGroup ctx (siteBracing site) mkBody CaseStyle mg)
  where
    keyword = keywordAt (spanOf hsCaseAnnCase) "case"

-- | A lambda, in any of its three spellings.
lambda ::
  ( Body b,
    Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
    Anno [LMatch GhcPs (LocatedA body)] ~ SrcSpanAnnLW,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  -- | The context.
  Ctx ->
  -- | Where it stands.
  Site ->
  -- | Which of the three spellings.
  HsLamVariant ->
  -- | Each body, as the alternatives hand it on.
  BodyOf body b ->
  -- | The alternatives.
  MatchGroup GhcPs (LocatedA body) ->
  Doc
lambda ctx site variant' mkBody mg = case keyword of
  Nothing -> matchGroup ctx (siteBracing site) mkBody LambdaStyle mg
  Just kw ->
    indent
      (underSite site)
      (txt kw)
      ( breakOrSpace
          <> matchGroup ctx (siteBracing site) mkBody LambdaCaseStyle mg
      )
  where
    keyword = case variant' of
      LamSingle -> Nothing
      LamCase -> Just "\\case"
      LamCases -> Just "\\cases"

-- | An @if@ expression or command.
ifThenElse ::
  Body b =>
  Ctx ->
  (LocatedA body -> b) ->
  AnnsIf ->
  LHsExpr GhcPs ->
  LocatedA body ->
  LocatedA body ->
  Doc
ifThenElse ctx bodyOf AnnsIf{aiIf, aiThen, aiElse} condition thenBody elseBody =
  refAt (spanOf aiIf) (txt "if")
    >+> space
      <> hsExpr ctx condition
      <> breakOrSpace
      <> branch (locA aiThen) "then" thenBody
      <> breakOrSpace
      <> branch (locA aiElse) "else" elseBody
  where
    branch written word body =
      keywordAt keywordSpan word
        >+> space
          <> layoutFrom
            ctx
            (keywordSpan <> spanOf body)
            (attach placement (printBody (bodyOf body)))
      where
        keywordSpan = spanOfSrcSpan written
        placement
          | commentPrintedBetween ctx keywordSpan (spanOf body) = Normal
          | remarkOnEachLine ctx keywordSpan (spanOf body) = Normal
          | otherwise = bodyPlacement (bodyOf body)

-- | A @let@ expression or command, with @in@ ending its line where it is
-- one of a chain written that way, and the next on the line under it.
letIn ::
  Body b =>
  Ctx ->
  (LocatedA body -> b) ->
  -- | Whether the body is another @let@.
  Bool ->
  (EpToken "let", EpToken "in") ->
  HsLocalBinds GhcPs ->
  LocatedA body ->
  Doc
letIn ctx bodyOf nested (letToken, inToken) binds body =
  align $
    written (spanOf letToken) (txt "let")
      <> space
      <> align
        ( localBinds ctx NoBrace binds
            <> foldMap (emptyAnchor . startOf) (spanOf inToken)
        )
      <> written (spanOf inToken) (separator <> txt "in")
      <> (if chained then breakOrSpace else space)
      <> align (printBody (bodyOf body))
  where
    chained = nested && not (sameLine (spanOf inToken) (spanOf body))
    separator = if chained then space else variant space (hardBreak <> txt " ")
    written s d = if chained then fence s d else d

-- | The bindings of a @let@ or a @where@.
localBinds ::
  -- | The context.
  Ctx ->
  -- | Whether the block may brace itself when flat.
  Bracing ->
  -- | The bindings.
  HsLocalBinds GhcPs ->
  Doc
localBinds ctx bracing = \case
  HsValBinds ann (ValBinds _ binds sigs) ->
    anchored ann . align . itemsOrBraces ann bracing (separatedByBlank ctx) $
      [ (spanOf item, rendered position item)
      | (position, item) <- positioned sorted
      ]
    where
      sorted = inSourceOrder [fmap (fmap Left) bound, fmap (fmap Right) sigs]
      bound = case al_brackets (anns ann) of
        ListBraces{} -> concatMap equations binds
        _ -> binds
      rendered position item =
        bracedByPosition position NoBrace $ \b ->
          at ctx item $ \case
            Left bind -> valDecl ctx b bind
            Right sig -> knotSig (ctxKnot ctx) ctx sig
  HsValBinds _ _ -> error "Tilia: renamer-only local bindings"
  HsIPBinds ann (IPBinds _ xs) ->
    anchored ann . itemsOrBraces ann bracing (separatedByBlank ctx) $
      [(spanOf x, at ctx x implicitBind) | x <- xs]
  EmptyLocalBinds _ -> mempty
  where
    implicitBind (IPBind _ n e) =
      at ctx n outputable
        >+> trailing (txt "=")
          <> attach placement (hsExpr ctx e)
      where
        placement
          | sameLine (spanOf n) (spanOf e) = exprHangs ctx (unLoc e)
          | otherwise = Normal
    anchored ann d = case ann of
      EpAnn{anns = AnnList{al_anchor, al_brackets}}
        | not (isZeroWidthSpan (locA al_anchor)) ->
            let held = spanOfSrcSpan (locA al_anchor)
             in maybe id located (held <> bracketsSpan al_brackets) $
                  layoutFrom ctx held d
      _ -> d

-- | The equations of a binding, each a binding of its own.
equations ::
  -- | The binding.
  LHsBind GhcPs ->
  [LHsBind GhcPs]
equations = \case
  L _ (FunBind x f (MG o (L l ms))) ->
    [L lm (FunBind x f (MG o (L l [m]))) | m@(L lm _) <- ms]
  b -> [b]

-- | Where a @do@ or @mdo@ was written.
doKeywordSpan :: AnnList EpaLocation -> Maybe Span
doKeywordSpan = spanOfSrcSpan . locA . al_rest

-- | Where the @where@ keyword of a group of local bindings was.
whereKeywordSpan :: HsLocalBinds GhcPs -> Maybe Span
whereKeywordSpan =
  spanOfSrcSpan . \case
    HsValBinds EpAnn{anns = AnnList{al_rest}} _ -> locA al_rest
    HsIPBinds EpAnn{anns = AnnList{al_rest}} _ -> locA al_rest
    EmptyLocalBinds _ -> noSrcSpan

-- | Does this @let@ or @where@ bind nothing at all?
isEmptyLocalBinds :: HsLocalBinds GhcPs -> Bool
isEmptyLocalBinds = \case
  EmptyLocalBinds _ -> True
  HsValBinds _ (ValBinds _ binds sigs) -> null binds && null sigs
  _ -> False

-- | An arrow-notation command.
hsCmd :: Ctx -> Site -> LHsCmd GhcPs -> Doc
hsCmd ctx site l = at ctx l (cmdBody ctx site)

-- | Every shape an arrow-notation command can take.
cmdBody :: Ctx -> Site -> HsCmd GhcPs -> Doc
cmdBody ctx site = \case
  HsCmdArrApp _ body input arrow rightToLeft ->
    let writtenFirst = if rightToLeft then body else input
        writtenSecond = if rightToLeft then input else body
     in hsExprIn ctx site writtenFirst
          >+> breakOrSpace
            <> ( txt (reserved ctx (arrowText arrow rightToLeft))
                   >+> attachBody
                     (bodyIn (ExprBody ctx) (siteBracing site) writtenSecond)
               )
  HsCmdArrForm ann form Prefix cmds ->
    spacedBracket
      0
      (closingFor site)
      (bracketSpans (al_brackets ann))
      (reserved ctx "(|")
      (reserved ctx "|)")
      applied
    where
      applied
        | null cmds = hsExpr ctx form
        | otherwise =
            hsExpr ctx form
              >+> breakOrSpace
                <> sepBy
                  breakOrSpace
                  (fmap (printBody . CmdTopBody ctx plainSite) cmds)
  HsCmdArrForm _ form Infix [l, r] -> cmdChain ctx site l form r
  HsCmdArrForm _ _ Infix _ ->
    error "Tilia: an infix command form always has exactly two operands"
  HsCmdApp _ cmd e ->
    hsCmd ctx site{siteFollowed = True} cmd
      >+> breakOrSpace
        <> hsExprIn ctx (withBracing (siteBracing site) plainSite) e
  HsCmdLam _ variant' mg -> lambda ctx site variant' (CmdBody ctx) mg
  HsCmdPar tokens c ->
    bracketWith
      (closingFor site)
      (pairSpans tokens)
      "("
      ")"
      (hsCmd ctx plainSite c)
  HsCmdCase anns e mg -> caseOf ctx site (CmdBody ctx) anns e mg
  HsCmdIf anns _ c t e ->
    ifThenElse ctx (bodyIn (CmdBody ctx) (siteBracing site)) anns c t e
  HsCmdLet tokens binds c ->
    letIn ctx (bodyIn (CmdBody ctx) (siteBracing site)) nested tokens binds c
    where
      nested = case unLoc c of
        HsCmdLet{} -> True
        _ -> False
  HsCmdDo anns es ->
    statements ctx site (CmdBody ctx) (keywordAt (doKeywordSpan anns) "do") es

-- | How the arrow of a command application is spelled.
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
  operatorChain
    ctx
    site
    (CmdTopBody ctx)
    isCmdDo
    (flattenAround splitCmd l op r)
  where
    splitCmd c = case unLoc c of
      HsCmdTop _ (L _ (HsCmdArrForm _ o Infix [a, b])) -> Just (a, o, b)
      _ -> Nothing
    isCmdDo c = case unLoc c of
      HsCmdTop _ (L _ HsCmdDo{}) -> True
      _ -> False

-- | A value binding.
valDecl :: Ctx -> Bracing -> HsBind GhcPs -> Doc
valDecl ctx bracing = \case
  FunBind _ funId funMatches ->
    matchGroup ctx bracing (ExprBody ctx) (FunctionStyle funId) funMatches
  PatBind _ p multAnn grhss ->
    match
      ctx
      bracing
      (ExprBody ctx)
      PatternBindStyle
      False
      multAnn
      mempty
      (Nothing, Nothing)
      [p]
      grhss
  PatSynBind _ psb -> patSynBind ctx psb
  VarBind{} -> error "Tilia: VarBind is introduced by the type checker"

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
  = -- | @| p = …@
    EqualsSign
  | -- | @| p -> …@
    RightArrow
  deriving (Eq, Show)

-- | A group of equations.
matchGroup ::
  ( Body b,
    Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO,
    Anno [LMatch GhcPs (LocatedA body)] ~ SrcSpanAnnLW,
    Anno (Match GhcPs (LocatedA body)) ~ SrcSpanAnnA
  ) =>
  -- | The context.
  Ctx ->
  -- | Whether the block may brace itself when flat.
  Bracing ->
  -- | Each body, as the equations hand it on.
  BodyOf body b ->
  -- | Which shape the equations take.
  MatchStyle ->
  -- | The equations.
  MatchGroup GhcPs (LocatedA body) ->
  Doc
matchGroup ctx bracing mkBody style MG{..}
  | isCaseStyle style, null (unLoc mg_alts) = txt "{}"
  | otherwise =
      itemsOrBraces (getLoc mg_alts) blockBracing (\_ _ -> False) $
        [(spanOf m, rendered x) | x@(_, m) <- positioned (unLoc mg_alts)]
  where
    (blockBracing, lastBracing)
      | isCaseStyle style = (bracing, NoBrace)
      | otherwise = (NoBrace, bracing)
    rendered (position, m) =
      bracedByPosition
        position
        lastBracing
        (at ctx m . renderMatch)
    renderMatch b m@Match{..} =
      match
        ctx
        b
        mkBody
        (adjustStyle m style)
        (isInfixMatch m)
        (HsUnannotated EpPatBind)
        (bangBeforeName m)
        (headParens m)
        (unLoc m_pats)
        m_grhss

-- | Put this equation's own name into the style it is printed with. A
-- style that carries no name is left as it is.
adjustStyle :: Match GhcPs body -> MatchStyle -> MatchStyle
adjustStyle m = \case
  FunctionStyle _ | FunRhs{mc_fun = f} <- m_ctxt m -> FunctionStyle f
  style -> style

-- | The @!@ or @~@ written in front of the name this equation defines.
bangBeforeName ::
  -- | The equation.
  Match GhcPs body ->
  Doc
bangBeforeName = \case
  Match{m_ctxt = FunRhs{mc_strictness, mc_an}} ->
    strictness (spanOf (afr_strict mc_an)) mc_strictness
  _ -> mempty

-- | Where the parentheses around this equation's head were written, where
-- it was written infix with more arguments after them.
headParens ::
  -- | The equation.
  Match GhcPs body ->
  (Maybe Span, Maybe Span)
headParens = \case
  Match{m_ctxt = FunRhs{mc_an}} ->
    outerBracketSpans (afr_opens mc_an) (afr_closes mc_an)
  _ -> (Nothing, Nothing)

-- | One equation: a head, a body, and possibly a @where@.
match ::
  (Body b, Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO) =>
  -- | The context.
  Ctx ->
  -- | Whether its last block takes braces.
  Bracing ->
  -- | How to print its body.
  BodyOf body b ->
  -- | What kind of equation it is.
  MatchStyle ->
  -- | Written infix?
  Bool ->
  -- | The multiplicity written on it.
  HsMultAnn GhcPs ->
  -- | The @!@ or @~@ in front of the name it defines.
  Doc ->
  -- | Where the parentheses around its head were written, where there are
  -- any.
  (Maybe Span, Maybe Span) ->
  -- | Its patterns.
  [LPat GhcPs] ->
  -- | Its right-hand side.
  GRHSs GhcPs (LocatedA body) ->
  Doc
match ctx bracing mkBody style isInfix multAnn strict parensAt pats GRHSs{..} =
  multiplicity (hsType ctx) multAnn
    <> multAnnGap
    <> strict
    <> case (style, pats) of
      (FunctionStyle n, _ : _) ->
        head'
          <> fromArguments
            ( headArguments
                (fromBool isInfix)
                (name ctx n)
                (fmap (hsPat ctx) pats)
            )
      (_, [])
        | not (isFunction style) ->
            ref rightHandSide <> indent 1 (ptr rightHandSide) whereClause
      (LambdaStyle, _) -> ref head' <> fromArguments head'
      (_, [p])
        | not (isFunction style) ->
            ref head' <> fromArguments (patArguments ctx p)
      _ -> ref head' <> fromArguments head'
  where
    fromArguments from =
      indent 1 (ptr from) rightHandSide <> indent 1 (ptr from) whereClause
    isFunction = \case
      FunctionStyle _ -> True
      _ -> False
    rightHandSide =
      separator
        <> layoutFrom
          ctx
          bodySpan
          (guarded (fmap alternative (NE.toList grhssGRHSs)))

    -- The first guard of an alternative of \cases with no patterns begins
    -- the alternative, so the rest go further in than it.
    guarded = \case
      a : as
        | null pats && isCaseStyle style ->
            a >+> foldMap (breakOrSpace <>) as
      as -> attach placement (sepBy breakOrSpace as)

    multAnnGap = case multAnn of
      HsUnannotated{} -> mempty
      _ -> space

    headSpan = case style of
      FunctionStyle n -> spanOf n <> patSpans
      _ -> patSpans
    patSpans = spansOf pats

    head' = case pats of
      [] -> case style of
        FunctionStyle n -> name ctx n
        _ -> mempty
      (headPat : tailPats) -> held . layoutFrom ctx headSpan $ case style of
        FunctionStyle n ->
          defHead
            1
            parensAt
            (fromBool isInfix)
            (name ctx n)
            ( zipWith
                (hsPatClosing ctx)
                ((if isInfix then 1 else 0) : repeat 0)
                pats
            )
        PatternBindStyle -> sepBy breakOrSpace rendered
        CaseStyle -> sepBy breakOrSpace rendered
        LambdaStyle ->
          txt "\\" <> lambdaGap headPat <> align (sepBy breakOrSpace rendered)
        LambdaCaseStyle
          | null tailPats -> hsPat ctx headPat
          | otherwise ->
              hsPat ctx headPat
                >+> breakOrSpace
                  <> sepBy breakOrSpace (fmap (hsPat ctx) tailPats)
      where
        rendered = fmap (hsPat ctx) pats
        held = case style of
          FunctionStyle _ -> id
          _ -> infrangible

    lambdaGap p = includeWhen (needsGap (unLoc p)) space
    needsGap p = case p of
      LazyPat{} -> True
      BangPat{} -> True
      SplicePat{} -> True
      InvisPat{} -> True
      NPat{} -> negative
      LitPat{} -> negative
      _ -> False
      where
        negative = T.isPrefixOf "-" (showGhc p)

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
        | any (longGuard . unLoc) grhssGRHSs
            || not (sameLine (Just spn) rhsSpan) ->
            Normal
      _ -> blockPlacement (bodyIn mkBody bracing) grhssGRHSs
    longGuard grhs = case guardsOf grhs of
      [] -> False
      [g] -> not (maybe True isSingleLine (spanOf g))
      _ -> True

    separator
      | hasGuards = mempty
      | otherwise = case style of
          FunctionStyle _ -> trailing (txt "=")
          PatternBindStyle -> trailing (txt "=")
          _ -> trailing (txt (reserved ctx "->"))

    alternative g =
      fence (spanOf g) $
        guardedRhs ctx placement bracing mkBody groupStyle g

    groupStyle
      | isCaseStyle style && hasGuards = RightArrow
      | otherwise = EqualsSign

    whereClause = case grhssLocalBinds of
      EmptyLocalBinds _ -> mempty
      binds
        | isEmptyLocalBinds binds -> breakOrSpace <> whereKeyword
        | otherwise ->
            breakOrSpace
              <> ( whereKeyword
                     >+> breakOrSpace
                       <> localBinds ctx bracing binds
                 )
        where
          whereKeyword = keywordAt (whereKeywordSpan binds) "where"

-- | Is this an alternative of a @case@ or a @\\case@?
isCaseStyle :: MatchStyle -> Bool
isCaseStyle = \case
  CaseStyle -> True
  LambdaCaseStyle -> True
  _ -> False

-- | Where an alternative was written, guards and all.
grhsSpan :: GRHS GhcPs (LocatedA body) -> Maybe Span
grhsSpan (GRHS _ guards body) = spanOf body <> spansOf guards

-- | The guards of an alternative.
guardsOf :: GRHS GhcPs body -> [GuardLStmt GhcPs]
guardsOf (GRHS _ guards _) = guards

-- | The placement of a body that is the whole of an equation.
blockPlacement ::
  Body b =>
  (LocatedA body -> b) ->
  NonEmpty (LGRHS GhcPs (LocatedA body)) ->
  Placement
blockPlacement bodyOf = \case
  L _ (GRHS _ _ body) :| [] -> bodyPlacement (bodyOf body)
  _ -> Normal

-- | One alternative of an equation: its guards, and what they guard.
guardedRhs ::
  (Body b, Anno (GRHS GhcPs (LocatedA body)) ~ EpAnnCO) =>
  Ctx ->
  -- | How the equation as a whole is placed.
  Placement ->
  -- | Bracing the body inherits.
  Bracing ->
  BodyOf body b ->
  GuardStyle ->
  LGRHS GhcPs (LocatedA body) ->
  Doc
guardedRhs ctx parentPlacement bracing mkBody style lgrhs@(L _ grhs) = case guards of
  [] -> atSpan ctx (grhsSpan grhs) (printBody bound)
  _ ->
    maybe id located (spanOf lgrhs) . layoutFrom ctx (grhsSpan grhs) $
      fence
        headSpan
        ( heldBesideTheHead
            ( bar
                <> equation (align (ref conditions))
                <> indent 1 (ptr bar) (trailing (txt (reserved ctx separator)))
            )
        )
        <> fence
          (fmap endOf separatorSpan <> spanOf body)
          ( equation
              (indent 1 (ptr base) (attach placement (printBody bound)))
          )
  where
    GRHS EpAnn{anns = GrhsAnn{ga_vbar, ga_sep}} guards body = grhs
    bar = refAt (spanOf =<< ga_vbar) (leading (txt "|"))
    conditions =
      commaSep
        (ctxCommaStyle ctx)
        (fmap (align . hsStmt ctx) (lastLetBraced guards))
    -- Wider steps put the body only one step past the bar, as Fourmolu
    -- does.
    (equation, base)
      | parentPlacement == Normal && narrowSteps =
          (id, conditions)
      | otherwise = (id, bar)
    heldBesideTheHead = if parentPlacement == Normal then id else infrangible
    narrowSteps = indentationColumns (ctxIndentation ctx) <= 2
    headSpan = (<>) <$> (spanOf =<< ga_vbar) <*> separatorSpan
    bound = bodyIn mkBody bracing body
    separator = case style of
      EqualsSign -> "="
      RightArrow -> "->"
    placement
      | sameLine separatorSpan (spanOf body),
        not (commentPrintedBetween ctx (spansOf guards) separatorSpan),
        not (remarkOnEachLine ctx (spansOf guards) separatorSpan) =
          bodyPlacement bound
      | otherwise = Normal
    separatorSpan = either spanOf spanOf ga_sep

-- | The qualifiers of a guard, a @let@ that ends them put between braces,
-- since what follows it on its line, the guard's @=@ or @->@, ends its
-- block only as a parse error does, and continues a signature it ends with.
lastLetBraced ::
  -- | The qualifiers, as written.
  [GuardLStmt GhcPs] ->
  [GuardLStmt GhcPs]
lastLetBraced guards = case unsnoc guards of
  Just (qualifiers, L l (LetStmt x binds)) ->
    qualifiers <> [L l (LetStmt x (betweenBraces binds))]
  _ -> guards
  where
    betweenBraces = \case
      HsValBinds ann bs -> HsValBinds (withBraces ann) bs
      HsIPBinds ann bs -> HsIPBinds (withBraces ann) bs
      bs -> bs
    withBraces ann = case al_brackets (anns ann) of
      ListBraces{} -> ann
      _ -> ann{anns = (anns ann){al_brackets = ListBraces noAnn noAnn}}

-- | A pattern synonym binding.
patSynBind ::
  -- | The context.
  Ctx ->
  -- | The binding.
  PatSynBind GhcPs GhcPs ->
  Doc
patSynBind ctx PSB{..} =
  txt "pattern" <> case psb_args of
    PrefixCon args ->
      space
        <> ( name ctx psb_id
               >+> layoutAcross
                 ctx
                 args
                 (argsAfterName (fmap (name ctx) args) (null args))
                 <> definition (spansOf args)
           )
    RecCon args ->
      space
        <> ( name ctx psb_id
               >+> layoutAcross
                 ctx
                 (vars args)
                 ( includeUnless (null args) breakOrNothing
                     <> bracketWith
                       0
                       (spanOf (ap_openc psb_ext), spanOf (ap_closec psb_ext))
                       "{"
                       "}"
                       ( commaSep
                           (ctxCommaStyle ctx)
                           (fmap (name ctx) (vars args))
                       )
                 )
                 <> definition (spansOf (vars args))
           )
      where
        vars = fmap recordPatSynPatVar
    InfixCon l r ->
      let left = name ctx l
       in layoutFrom
            ctx
            (spanOf l <> spanOf r)
            ( space
                <> ref left
                <> indent
                  1
                  (ptr left)
                  ( attachOperator
                      ctx
                      Normal
                      id
                      (spanOf l)
                      (spanOf psb_id)
                      (spanOf r)
                      (name ctx psb_id)
                      (name ctx r)
                  )
            )
            <> indent 1 (ptr left) (definition (spanOf l <> spanOf r))
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
            <> ( txt "where"
                   >+> breakOrSpace
                     <> matchGroup
                       ctx
                       NoBrace
                       (ExprBody ctx)
                       (FunctionStyle psb_id)
                       mg
               )
      where
        rhs arrow =
          layoutFrom ctx (fmap endOf headSpan <> spanOf psb_def) $
            txt (reserved ctx arrow) <> breakOrSpace <> hsPat ctx psb_def
        headSpan =
          spanOf psb_id <> argSpans <> foldMap spanOf (ap_closec psb_ext)

-- | An untyped splice, either @$x@ or a quasi-quotation.
untypedSplice ::
  -- | The context.
  Ctx ->
  -- | How it is written.
  SpliceDecoration ->
  -- | The splice.
  HsUntypedSplice GhcPs ->
  Doc
untypedSplice ctx deco = \case
  HsUntypedSpliceExpr tok e -> spliceTH ctx (Isn't #typed) (spanOf tok) e deco
  HsQuasiQuote _ quoter str ->
    txt "["
      <> name ctx quoter
      <> txt "|"
      <> verbatim (above <> indentation)
      <> maybe id located lastLine (txt rest <> txt "|]")
    where
      (above, final) = T.breakOnEnd "\n" (T.pack (unpackFS (unLoc str)))
      (indentation, rest) = T.span isSpace final
      lastLine
        | T.null above = Nothing
        | otherwise =
            (\s -> mkSpan (spanEndLine s, contentColumn final) (endPoint s))
              <$> spanOf str

-- | A Template Haskell splice, typed or not.
spliceTH ::
  -- | The context.
  Ctx ->
  -- | Is it typed?
  Choice "typed" ->
  -- | Where its dollar was written.
  Maybe Span ->
  -- | What is spliced.
  LHsExpr GhcPs ->
  -- | How it is written.
  SpliceDecoration ->
  Doc
spliceTH ctx typed dollar e = \case
  DollarSplice ->
    keywordAt dollar (if isTrue typed then "$$" else "$") <> spliced
  BareSplice -> spliced
  where
    spliced = at ctx e (align . exprBody ctx plainSite (spanOf e))

-- | A Template Haskell quotation.
quotation :: Ctx -> Maybe Span -> HsQuote GhcPs -> Doc
quotation ctx here = \case
  ExpBr (bracketAnn, close) e
    | BracketNoE{} <- bracketAnn,
      extensionOn ctx UnicodeSyntax ->
        quoted here brackets "⟦" "⟧" e (hsExpr ctx e)
    | otherwise -> quotedAs brackets flavour e (hsExpr ctx e)
    where
      brackets = (open, spanOf close)
      (open, flavour) = case bracketAnn of
        BracketNoE t -> (spanOf t, "")
        BracketHasE t -> (spanOf t, "e")
  PatBr brackets p -> quotedAs (pairSpans brackets) "p" p (hsPat ctx p)
  DecBrL (open, close, _) decls ->
    quotedAs
      (pairSpans (open, close))
      "d"
      decls
      (knotDecls (ctxKnot ctx) ctx Free decls)
  DecBrG _ _ -> error "Tilia: DecBrG is produced by the renamer"
  TypBr brackets ty -> quotedAs (pairSpans brackets) "t" ty (hsType ctx ty)
  VarBr _ isSingle n -> txt (if isSingle then "'" else "''") <> name ctx n
  where
    quotedAs ::
      Data a => (Maybe Span, Maybe Span) -> Text -> a -> Doc -> Doc
    quotedAs brackets flavour =
      quoted here brackets ("[" <> flavour <> "|") "|]"

-- | The body of a quotation between its brackets, indented where it breaks,
-- and spaced from them where it may end with a symbol character.
quoted ::
  Data a =>
  -- | Where the quotation is written.
  Maybe Span ->
  -- | Where its brackets were written.
  (Maybe Span, Maybe Span) ->
  -- | The opening bracket.
  Text ->
  -- | The closing bracket.
  Text ->
  -- | What is quoted.
  a ->
  -- | The body.
  Doc ->
  Doc
quoted here (o, c) opener closer x body =
  openingAt o opener
    >+> gap
      <> insideBrackets here body
      <> gap
      <> closingAt c closer
  where
    gap = if risky then breakOrSpace else breakOrNothing
    risky =
      any isStar (listify (const True :: HsType GhcPs -> Bool) x)
        || any isAbstract (listify (const True :: FamilyInfo GhcPs -> Bool) x)
        || not (null (listify (const True :: FixitySig GhcPs -> Bool) x))
    isStar = \case
      HsStarTy{} -> True
      _ -> False
    isAbstract = \case
      ClosedTypeFamily Nothing -> True
      _ -> False

-- | A block 'braced' where it was written between braces, as the region
-- they enclose, and 'items' otherwise.
itemsOrBraces ::
  -- | The block's annotation, which holds its braces.
  EpAnn (AnnList a) ->
  -- | Whether the block may brace itself when flat.
  Bracing ->
  -- | Was there an empty line between two items?
  (Maybe Span -> Maybe Span -> Bool) ->
  -- | Where each item was, and what it prints as.
  [(Maybe Span, Doc)] ->
  Doc
itemsOrBraces ann bracing blank = case al_brackets (anns ann) of
  brackets@ListBraces{} ->
    maybe id located (bracketsSpan brackets)
      . braced (bracketSpans brackets) blank
  _ -> items bracing . keepBlanks blank

-- | Print an item of a block braced by its position: the last one keeps
-- the block's bracing, and any other may brace itself when the block is
-- flat.
bracedByPosition ::
  -- | Where the item is in the block.
  Position ->
  -- | The block's bracing.
  Bracing ->
  -- | How to print the item with a given bracing.
  (Bracing -> Doc) ->
  Doc
bracedByPosition position bracing render = case position of
  Last -> render bracing
  Only -> render bracing
  _ -> mayBraceWhenFlat bracing render

-- | Let a block brace itself when the surrounding layout is flat.
mayBraceWhenFlat :: Bracing -> (Bracing -> Doc) -> Doc
mayBraceWhenFlat whenBroken render =
  variant (render MayBrace) (render whenBroken)

----------------------------------------------------------------------------
-- Bodies

-- | Something that can appear as the body of an enclosing construct.
class Body a where
  -- | Print it.
  printBody :: a -> Doc

  -- | Whether it absorbs its own line break.
  bodyPlacement :: a -> Placement

-- | Print a body and join it to whatever precedes it.
attachBody ::
  Body a =>
  -- | The body.
  a ->
  Doc
attachBody x = attach (bodyPlacement x) (printBody x)

-- | An expression standing as the body of an enclosing construct.
data ExprBody = ExprBody Ctx Site (LHsExpr GhcPs)

instance Body ExprBody where
  printBody (ExprBody ctx site e) = hsExprIn ctx site e
  bodyPlacement (ExprBody ctx _ e) = exprHangs ctx (unLoc e)

-- | A command standing as the body of an enclosing construct.
data CmdBody = CmdBody Ctx Site (LHsCmd GhcPs)

instance Body CmdBody where
  printBody (CmdBody ctx site c) = hsCmd ctx site c
  bodyPlacement (CmdBody _ _ c) = cmdHangs (unLoc c)

-- | A command at the top of an arrow form.
data CmdTopBody = CmdTopBody Ctx Site (LHsCmdTop GhcPs)

instance Body CmdTopBody where
  printBody (CmdTopBody ctx site l) = at ctx l (cmdTop ctx site)
  bodyPlacement (CmdTopBody _ _ l) = cmdTopHangs (unLoc l)

-- | Does this expression absorb the line break that introduces it?
exprHangs ::
  -- | The context.
  Ctx ->
  -- | The expression.
  HsExpr GhcPs ->
  Placement
exprHangs ctx = \case
  HsDo _ (DoExpr _) _ -> Hanging
  HsDo _ (MDoExpr _) _ -> Hanging
  HsCase{} -> Hanging
  HsLam _ lamVariant mg -> case lamVariant of
    LamCase -> Hanging
    LamCases -> Hanging
    LamSingle -> case mg of
      MG _ (L _ [L _ (Match _ _ (L _ ps@(_ : _)) _)])
        | maybe False isSingleLine (spansOf ps) -> Hanging
      _ -> Normal
  HsProc _ p _
    | maybe False isSingleLine (spanOf p) -> Hanging
    | otherwise -> Normal
  HsApp _ _ y -> exprHangs ctx (unLoc y)
  OpApp _ x op y
    | Just n <- operatorName op,
      occNameString (rdrNameOcc n) == "$" ->
        exprHangs ctx (unLoc y)
    | doWithinAStep ctx isDoBlock x -> Normal
    | otherwise -> exprHangs ctx (unLoc x)
  _ -> Normal

-- | Does this expression end with a layout block, which takes in what is
-- printed after it on its last line?
endsWithBlock :: HsExpr GhcPs -> Bool
endsWithBlock = \case
  HsDo _ (DoExpr _) _ -> True
  HsDo _ (MDoExpr _) _ -> True
  HsCase{} -> True
  HsLam _ LamCase _ -> True
  HsLam _ LamCases _ -> True
  HsApp _ _ y -> endsWithBlock (unLoc y)
  OpApp _ _ _ y -> endsWithBlock (unLoc y)
  _ -> False

-- | Does this command absorb the line break that introduces it?
cmdHangs :: HsCmd GhcPs -> Placement
cmdHangs = \case
  HsCmdDo{} -> Hanging
  HsCmdCase{} -> Hanging
  HsCmdLam{} -> Hanging
  _ -> Normal

-- | Does this command at the top of an arrow form absorb the line break
-- that introduces it?
cmdTopHangs :: HsCmdTop GhcPs -> Placement
cmdTopHangs (HsCmdTop _ c) = cmdHangs (unLoc c)

-- | The name of an operator, when the expression standing as one is a name.
operatorName :: LHsExpr GhcPs -> Maybe RdrName
operatorName e = case unLoc e of
  HsVar _ (L _ n) -> Just n
  _ -> Nothing

-- | Is this operator the minus, the one operator that can also begin an
-- expression?
isMinus ::
  -- | The operator.
  LHsExpr GhcPs ->
  Bool
isMinus o = case operatorName o of
  Just (Unqual n) -> occNameString n == "-"
  _ -> False
