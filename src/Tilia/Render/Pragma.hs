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
    spelledAsWritten,
    overlapMode,
    warnDecls,
    warningTxt,
  )
where

import Data.Char (toUpper)
import Data.List (isInfixOf)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.FastString (unpackFS)
import GHC.Hs
import GHC.Types.Basic hiding (overlapMode)
import GHC.Types.SourceText
import GHC.Types.SrcLoc (unLoc)
import GHC.Unit.Module.Warnings
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Render.Literal (stringLiteral, writtenString)
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

-- | Which flavour of inlining was asked for, @INLINABLE@ spelled as written.
inlineSpec :: InlineSpec -> Doc
inlineSpec = \case
  Inline _ -> txt "INLINE"
  Inlinable src -> txt (spelledAsWritten "INLINABLE" "INLINEABLE" src)
  NoInline _ -> txt "NOINLINE"
  Opaque _ -> txt "OPAQUE"
  NoUserInlinePrag -> mempty

-- | Whichever of two spellings of a pragma's name its source text uses, the
-- first where it has none.
spelledAsWritten :: Text -> Text -> SourceText -> Text
spelledAsWritten usual other = \case
  SourceText s
    | T.unpack other `isInfixOf` fmap toUpper (unpackFS s) -> other
  _ -> usual

-- | The overlap pragma of an instance.
overlapMode :: Ctx -> Maybe (LocatedP OverlapMode) -> Maybe Doc
overlapMode ctx mode = do
  m <- mode
  keyword <- spelled (unLoc m)
  pure (at ctx m (const (txt (braced keyword))))
  where
    braced keyword = "{-# " <> keyword <> " #-}"
    spelled = \case
      Overlappable{} -> Just "OVERLAPPABLE"
      Overlapping{} -> Just "OVERLAPPING"
      Overlaps{} -> Just "OVERLAPS"
      Incoherent{} -> Just "INCOHERENT"
      _ -> Nothing

-- | A @WARNING@ or @DEPRECATED@ declaration.
warnDecls :: Ctx -> WarnDecls GhcPs -> Doc
warnDecls ctx (Warnings (_, opening) warnings) =
  layoutAcross ctx warnings
    . pragma keyword
    . indent
    $ sepBy (txt ";" <> breakOrSpace) (fmap (at_ ctx (warned ctx)) warnings)
  where
    keyword = case opening of
      SourceText s
        | "DEPRECATED" `isInfixOf` fmap toUpper (unpackFS s) -> "DEPRECATED"
      _ -> "WARNING"

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
    named (unLoc -> InWarningCategory{..}) =
      txt "in" <> space <> written iwc_st iwc_wc <> space
    written (SourceText s) _ = stringLiteral s
    written NoSourceText wc = txt ("\"" <> showGhc (unLoc wc) <> "\"")

-- | One message is written bare; several go in a list.
literalList :: [LocatedE StringLiteral] -> Doc
literalList = \case
  [l] -> writtenString (unLoc l)
  ls -> brackets (commaSep (fmap (writtenString . unLoc) ls))
