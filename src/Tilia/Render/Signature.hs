{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Signatures, and the pragmas that are written like them.
module Tilia.Render.Signature
  ( sigDecl,
    standaloneKindSig,
    ruleDecls,
    specialisedName,
  )
where

import Data.Choice (Choice, isTrue, pattern Do, pattern Don't)
import Data.Maybe (maybeToList)
import GHC.Data.BooleanFormula hiding (isTrue)
import GHC.Hs
import GHC.Types.Basic
  ( Activation (..),
    InlinePragma (..),
    InlineSpec (..),
    RuleMatchInfo (..),
    RuleName,
  )
import GHC.Types.Fixity (Fixity (..), FixityDirection (..))
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Expression (hsExpr)
import Tilia.Render.Name
import Tilia.Render.Pragma
import Tilia.Render.Type
import Tilia.Span (startOf)
import Tilia.Span.Ghc (tokenSpan)

-- | A signature declaration.
sigDecl :: Ctx -> Sig GhcPs -> Doc
sigDecl ctx = \case
  TypeSig _ names hswc -> typeSig ctx (Do #indentTail) names (hswc_body hswc)
  PatSynSig _ names sigType -> patSynSig ctx names sigType
  ClassOpSig _ isDefault names sigType ->
    includeWhen isDefault (txt "default" <> space) <> typeSig ctx (Do #indentTail) names sigType
  FixSig _ sig -> fixitySig ctx sig
  InlineSig _ n prag -> inlineSig ctx n prag
  SpecSig _ n types prag ->
    specialiseSig ctx Nothing (noLocA (HsVar NoExtField n)) types prag
  SpecSigE _ binders e prag -> specialiseSigE ctx binders e prag
  SpecInstSig _ sigType ->
    pragma "SPECIALIZE instance" (indent (hsSigType ctx sigType))
  MinimalSig _ formula ->
    at ctx formula (pragma "MINIMAL" . indent . booleanFormula ctx)
  CompleteMatchSig _ names ty -> completeSig ctx names ty
  SCCFunSig _ n literal -> sccSig ctx n literal

-- | @f, g :: t@.
typeSig ::
  -- | The context.
  Ctx ->
  -- | Indent the names after the first?
  Choice "indentTail" ->
  -- | The names the signature is about.
  [LocatedN RdrName] ->
  -- | The type they are given.
  LHsSigType GhcPs ->
  Doc
typeSig _ _ [] _ = mempty
typeSig ctx indentTail (n : ns) sigType
  | null ns = name ctx n <> typeAscription ctx sigType
  | otherwise =
      name ctx n
        <> nest
          (if isTrue indentTail then 1 else 0)
          ( comma
              <> breakOrSpace
              <> commaSep (fmap (name ctx) ns)
              <> typeAscription ctx sigType
          )

-- | @pattern P :: t@.
patSynSig :: Ctx -> [LocatedN RdrName] -> LHsSigType GhcPs -> Doc
patSynSig ctx names sigType
  | length names > 1 = txt "pattern" <> breakOrSpace <> indent body
  | otherwise = txt "pattern" <> space <> body
  where
    body = typeSig ctx (Don't #indentTail) names sigType

-- | @infixl 5 <+>@.
fixitySig :: Ctx -> FixitySig GhcPs -> Doc
fixitySig ctx (FixitySig namespace names (Fixity precedence direction)) =
  txt keyword
    <> space
    <> outputable precedence
    <> space
    <> namespaceSpec namespace
    <> align (commaSep (fmap (name ctx) names))
  where
    keyword = case direction of
      InfixL -> "infixl"
      InfixR -> "infixr"
      InfixN -> "infix"

-- | An @INLINE@ or @NOINLINE@ pragma.
inlineSig :: Ctx -> LocatedN RdrName -> InlinePragma -> Doc
inlineSig ctx n InlinePragma {..} =
  pragmaBrackets $
    inlineSpec inl_inline
      <> space
      <> conLike
      <> space
      <> includeUnless (inl_act == NeverActive) (activation inl_act)
      <> space
      <> name ctx n
  where
    conLike = case inl_rule of
      ConLike -> txt "CONLIKE"
      FunLike -> mempty

-- | A @SPECIALIZE@ pragma.
specialiseSig ::
  Ctx ->
  Maybe (RuleBndrs GhcPs) ->
  LHsExpr GhcPs ->
  [LHsSigType GhcPs] ->
  InlinePragma ->
  Doc
specialiseSig ctx binders target types InlinePragma {..} =
  pragmaBrackets $
    txt "SPECIALIZE"
      <> space
      <> inlineSpec inl_inline
      <> space
      <> phase
      <> indent
        ( space
            <> foldMap (\bs -> ruleBinders ctx bs <> space) binders
            <> hsExpr ctx target
            <> includeUnless
              (null types)
              (joinedBy "::" <> commaSep (fmap (hsSigType ctx) types))
        )
  where
    -- A pragma that says neither when to inline nor whether to is saying
    -- nothing, so the phase is left off rather than printed as @[~]@.
    phase = case (inl_inline, inl_act) of
      (NoInline _, NeverActive) -> mempty
      _ -> activation inl_act

-- | A @SPECIALIZE@ pragma written as an expression.
specialiseSigE ::
  Ctx ->
  RuleBndrs GhcPs ->
  LHsExpr GhcPs ->
  InlinePragma ->
  Doc
specialiseSigE ctx binders e =
  specialiseSig ctx (Just binders) target (maybeToList sigTy)
  where
    (target, sigTy) = specBody e

-- | A @SPECIALIZE@ expression without the type ascription it may carry.
specBody :: LHsExpr GhcPs -> (LHsExpr GhcPs, Maybe (LHsSigType GhcPs))
specBody = \case
  L _ (ExprWithTySig _ e HsWC {hswc_body}) -> (e, Just hswc_body)
  e -> (e, Nothing)

-- | The function a @SPECIALIZE@ expression applies, if it applies one.
--
-- The parser takes any expression at all here and leaves it to the renamer
-- to insist on a head variable, so a module on its way to being rejected
-- reaches us with expressions like @let x = 2 in f x@ in this position.
specHead :: LHsExpr GhcPs -> Maybe (LocatedN RdrName)
specHead (L _ e) = case e of
  HsVar _ n -> Just n
  HsApp _ f _ -> specHead f
  HsAppType _ f _ -> specHead f
  _ -> Nothing

-- | The name a @SPECIALIZE@ pragma is about, for grouping declarations.
specialisedName :: Sig GhcPs -> Maybe RdrName
specialisedName = \case
  SpecSig _ (L _ n) _ _ -> Just n
  SpecSigE _ _ e _ -> unLoc <$> specHead (fst (specBody e))
  _ -> Nothing

-- | The formula a @MINIMAL@ pragma is written with.
booleanFormula :: Ctx -> BooleanFormula GhcPs -> Doc
booleanFormula ctx = \case
  Var n -> name ctx n
  And xs -> align (commaSep (fmap (at_ ctx (booleanFormula ctx)) xs))
  Or xs ->
    align (sepBy (breakOrSpace <> txt "|" <> space) (fmap (at_ ctx (booleanFormula ctx)) xs))
  Parens l -> at ctx l (parens . booleanFormula ctx)

-- | A @COMPLETE@ pragma.
completeSig :: Ctx -> [LIdP GhcPs] -> Maybe (LocatedN RdrName) -> Doc
completeSig ctx names ty =
  layoutAcross ctx names . pragma "COMPLETE" . indent $
    commaSep (fmap (name ctx) names)
      <> foldMap
        (\t -> joinedBy "::" <> indent (name ctx t))
        ty

-- | An @SCC@ pragma.
sccSig :: Ctx -> LocatedN RdrName -> Maybe (XRec GhcPs StringLiteral) -> Doc
sccSig ctx n literal =
  pragma "SCC" . indent $
    name ctx n <> foldMap (\l -> breakOrSpace <> outputable l) literal

-- | @type T :: k@.
standaloneKindSig :: Ctx -> StandaloneKindSig GhcPs -> Doc
standaloneKindSig ctx (StandaloneKindSig _ n sigTy) =
  txt "type"
    <> indent
      ( space
          <> name ctx n
          <> joinedBy "::"
          <> hsSigType ctx sigTy
      )

-- | A @RULES@ block.
--
-- The closing @#-\}@ is given an anchor of its own, so that a comment
-- written after the last rule and before it stays inside the pragma. There
-- is nothing else down there for such a comment to attach to, and outside
-- the braces it would read as a remark on whatever follows the block.
ruleDecls :: Ctx -> RuleDecls GhcPs -> Doc
ruleDecls ctx (HsRules ((_, close), _) rules) =
  pragma "RULES" $
    sepBy breakOrSpace (fmap (align . at_ ctx (ruleDecl ctx)) rules)
      <> foldMap (emptyAnchor . startOf) (tokenSpan close)

-- | One rule of a @RULES@ block.
ruleDecl :: Ctx -> RuleDecl GhcPs -> Doc
ruleDecl ctx (HsRule _ ruleName phase binders lhs rhs) =
  at ctx ruleName ruleNameLiteral
    <> space
    <> activation phase
    <> space
    <> ruleBinders ctx binders
    <> breakOrSpace
    <> indent
      ( hsExpr ctx lhs
          <> space
          <> txt "="
          <> indent (breakOrSpace <> hsExpr ctx rhs)
      )

-- | A rule's name is a string literal, and printing it as one is what puts
-- the quotes back.
ruleNameLiteral :: RuleName -> Doc
ruleNameLiteral n = outputable (HsString NoSourceText n :: HsLit GhcPs)

-- | The @forall@s a rule or a @SPECIALIZE@ pragma binds.
ruleBinders :: Ctx -> RuleBndrs GhcPs -> Doc
ruleBinders ctx (RuleBndrs HsRuleBndrsAnn {..} tyvars binders) =
  foldMap
    (\xs -> forallBndrs ctx Invisible (tyVarBndr ctx) xs <> space)
    tyvars
    <> case rb_tmanns of
      Nothing -> mempty
      Just _ -> forallBndrs ctx Invisible (ruleBinder ctx) binders

-- | One variable a rule's @forall@ binds.
ruleBinder :: Ctx -> RuleBndr GhcPs -> Doc
ruleBinder ctx = \case
  RuleBndr _ n -> name ctx n
  RuleBndrSig _ n HsPS {..} ->
    parens (name ctx n <> typeAscription ctx (asSigType hsps_body))
