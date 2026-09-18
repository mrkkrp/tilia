{-# LANGUAGE LambdaCase #-}

-- | Hanging vs non-hanging constructs.
module Tilia.Render.Body
  ( ExprBody (..),
    CmdBody (..),
    CmdTopBody (..),
    exprHangs,
    operatorName,
    cmdTopHangs,
  )
where

import GHC.Hs
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName, rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Doc.Body
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Span
import Tilia.Span.Ghc

-- | An expression standing as the body of an enclosing construct.
data ExprBody = ExprBody Ctx Site (LHsExpr GhcPs)

instance Body ExprBody where
  printBody (ExprBody ctx site e) = knotExpr (ctxKnot ctx) ctx site e
  bodyPlacement (ExprBody _ _ e) = exprHangs (unLoc e)

-- | A command standing as the body of an enclosing construct.
data CmdBody = CmdBody Ctx Site (LHsCmd GhcPs)

instance Body CmdBody where
  printBody (CmdBody ctx site c) = knotCmd (ctxKnot ctx) ctx site c
  bodyPlacement (CmdBody _ _ c) = cmdHangs (unLoc c)

-- | A command at the top of an arrow form.
data CmdTopBody = CmdTopBody Ctx Site (LHsCmdTop GhcPs)

instance Body CmdTopBody where
  printBody (CmdTopBody ctx site l) =
    at ctx l (\(HsCmdTop _ cmd) -> knotCmd (ctxKnot ctx) ctx site cmd)
  bodyPlacement (CmdTopBody _ _ l) = cmdTopHangs (unLoc l)

-- | Does this expression absorb the line break that introduces it?
exprHangs :: HsExpr GhcPs -> Placement
exprHangs = \case
  HsDo _ (DoExpr _) _ -> Hanging
  HsDo _ (MDoExpr _) _ -> Hanging
  HsCase {} -> Hanging
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
  HsApp _ _ y -> exprHangs (unLoc y)
  OpApp _ _ op y
    | Just n <- operatorName op,
      occNameString (rdrNameOcc n) == "$" ->
        exprHangs (unLoc y)
  _ -> Normal

-- | Does this command absorb the line break that introduces it?
cmdHangs :: HsCmd GhcPs -> Placement
cmdHangs = \case
  HsCmdDo {} -> Hanging
  HsCmdCase {} -> Hanging
  HsCmdLam {} -> Hanging
  _ -> Normal

cmdTopHangs :: HsCmdTop GhcPs -> Placement
cmdTopHangs (HsCmdTop _ c) = cmdHangs (unLoc c)

-- | The name of an operator, when the expression standing as one is a name.
operatorName :: LHsExpr GhcPs -> Maybe RdrName
operatorName e = case unLoc e of
  HsVar _ (L _ n) -> Just n
  _ -> Nothing
