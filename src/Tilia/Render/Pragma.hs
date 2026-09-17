{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- | The @{-# … #-}@ annotations that appear among declarations.
module Tilia.Render.Pragma
  ( pragmaBrackets,
    pragma,
    activation,
    inlineSpec,
    overlapMode,
    warnDecls,
    warningTxt,
  )
where

import Data.Text (Text)
import GHC.Hs
import GHC.Types.Basic hiding (overlapMode)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import GHC.Unit.Module.Warnings
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Name

-- | Wrap a body in pragma braces.
pragmaBrackets :: Doc -> Doc
pragmaBrackets body =
  align (txt "{-#" <> space <> body <> breakOrSpace <> indent (txt "#-}"))

-- | A named pragma with a body.
pragma :: Text -> Doc -> Doc
pragma pragmaName body =
  pragmaBrackets (txt pragmaName <> breakOrSpace <> body)

-- | The phase control of an @INLINE@ or @RULES@ pragma.
activation :: Activation -> Doc
activation = \case
  NeverActive -> txt "[~]"
  AlwaysActive -> mempty
  ActiveBefore _ n -> txt "[~" <> outputable n <> txt "]"
  ActiveAfter _ n -> txt "[" <> outputable n <> txt "]"
  FinalActive -> error "Tilia: FinalActive is not expected in parsed source"

-- | Which flavour of inlining was asked for.
inlineSpec :: InlineSpec -> Doc
inlineSpec = \case
  Inline _ -> txt "INLINE"
  Inlinable _ -> txt "INLINEABLE"
  NoInline _ -> txt "NOINLINE"
  Opaque _ -> txt "OPAQUE"
  NoUserInlinePrag -> mempty

-- | The overlap pragma of an instance, and the separator after it.
overlapMode :: Maybe (LocatedP OverlapMode) -> Maybe Doc
overlapMode mode = txt . braced <$> (spelled . unLoc =<< mode)
  where
    braced keyword = "{-# " <> keyword <> " #-}"
    spelled = \case
      Overlappable {} -> Just "OVERLAPPABLE"
      Overlapping {} -> Just "OVERLAPPING"
      Overlaps {} -> Just "OVERLAPS"
      Incoherent {} -> Just "INCOHERENT"
      _ -> Nothing

-- | A @WARNING@ or @DEPRECATED@ declaration.
warnDecls :: Ctx -> WarnDecls GhcPs -> Doc
warnDecls ctx (Warnings _ warnings) = case warnings of
  [] -> mempty
  (L _ (Warning _ _ wtxt) : _) ->
    layoutAcross ctx warnings
      . pragma (keywordOf wtxt)
      . indent
      $ sepBy (txt ";" <> breakOrSpace) (fmap (at_ ctx (warned ctx)) warnings)
  where
    keywordOf wtxt = let (keyword, _, _) = warningParts wtxt in keyword

-- | One of the things a warning declaration names.
warned :: Ctx -> WarnDecl GhcPs -> Doc
warned ctx (Warning (namespace, _) names wtxt) =
  category
    <> namespaceSpec namespace
    <> commaSep (fmap (name ctx) names)
    <> breakOrSpace
    <> literalList literals
  where
    (_, category, literals) = warningParts wtxt

-- | A warning attached to a name in an export list or to an instance.
warningTxt :: WarningTxt GhcPs -> Doc
warningTxt wtxt =
  indent (pragma keyword (indent (category <> literalList literals)))
  where
    (keyword, category, literals) = warningParts wtxt

-- | Which keyword introduces a warning, which category it is filed under,
-- and what it says.
--
-- The keyword is written once for a whole declaration even when it names
-- several things, whereas the category belongs to each of them separately.
-- That is why the two do not come back as one piece of text.
warningParts :: WarningTxt GhcPs -> (Text, Doc, [LocatedE StringLiteral])
warningParts = \case
  DeprecatedTxt _ literals -> ("DEPRECATED", mempty, said literals)
  WarningTxt category _ literals ->
    ("WARNING", foldMap named category, said literals)
  where
    said = fmap (fmap hsDocString)
    named (unLoc -> InWarningCategory {..}) =
      txt ("in \"" <> showGhc (unLoc iwc_wc) <> "\"") <> space

-- | One message is written bare; several go in a list.
literalList :: [LocatedE StringLiteral] -> Doc
literalList = \case
  [l] -> outputable l
  ls -> brackets (commaSep (fmap outputable ls))
