{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- | Names, and the decorations the author put around them.
module Tilia.Render.Name
  ( -- * Rendering anything GHC can show
    outputable,
    showGhc,
    sourceText,

    -- * Names
    name,
    moduleHeadName,
    wrappedName,
    namespaceSpec,
    multiplicity,

    -- * Definition heads
    defHead,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs
import GHC.LanguageExtensions.Type (Extension (..))
import GHC.Types.Name.Occurrence (OccName, occNameString)
import GHC.Types.Name.Reader
import GHC.Types.SourceText
import GHC.Types.SrcLoc (getLoc)
import GHC.Utils.Outputable (Outputable, ppr, showSDocUnsafe)
import Tilia.Doc.Combinators
import Tilia.Render.Context
import Tilia.Source (SourceType (..))
import Tilia.Span
import Tilia.Span.Ghc

-- | Anything GHC knows how to show.
--
-- For the leaves that have no structure worth walking—numeric literals,
-- occurrence names, calling conventions. Never for anything that might need
-- a line break inside it, since the result is emitted as one fragment.
outputable :: (Outputable a) => a -> Doc
outputable = txt . showGhc

-- | The text GHC would show for something.
showGhc :: (Outputable a) => a -> Text
showGhc = T.pack . showSDocUnsafe . ppr

-- | The text the author wrote, when GHC kept it.
sourceText :: SourceText -> Doc
sourceText = \case
  NoSourceText -> mempty
  SourceText s -> outputable s

----------------------------------------------------------------------------
-- Names

-- | A name, with whatever the author wrapped it in.
name :: Ctx -> LocatedN RdrName -> Doc
name ctx l = at ctx l $ \x -> adorn ctx (spanOf l) x (getLoc l) (bareName x)

-- | The name itself, with nothing around it.
bareName :: RdrName -> Doc
bareName = \case
  Unqual occName -> outputable occName
  Qual mname occName -> qualifiedName mname occName
  Orig _ occName -> outputable occName
  Exact n -> outputable n

-- | Put back whatever brackets, backticks or ticks the author used.
adorn :: Ctx -> Maybe Span -> RdrName -> EpAnn NameAnn -> Doc -> Doc
adorn ctx here x = go
  where
    go EpAnn {anns} = case anns of
      -- A promotion tick, with whatever the name carries under it.
      NameAnnQuote {nann_quoted} -> (txt "'" <>) . go nann_quoted
      -- The empty unboxed sum and the empty list are written out whole:
      -- there is no name under the brackets to print.
      NameAnnOnly {nann_adornment = NameParensHash {}} -> const (txt "(# #)")
      NameAnnOnly {nann_adornment = NameSquare {}} ->
        const (txt "[" <> insideBrackets here mempty <> txt "]")
      -- @->@ is the one name that is a keyword as well, and the parentheses
      -- are recorded on their own rather than as an adornment.
      NameAnnRArrow {nann_mopen = Just _} -> inParens
      -- The name inside the brackets is claimed separately from the
      -- brackets themselves, so that a comment written against it—@( {-
      -- here -} :+: )@—is put where it was written rather than after the
      -- closing bracket.
      NameAnn {nann_adornment, nann_name} -> case nann_adornment of
        NameParens {} -> inParens . spaceOutHash . itsOwn nann_name
        NameBackquotes {} -> backticks . itsOwn nann_name
        _ -> itsOwn nann_name
      _ -> id

    itsOwn = atSpan ctx . annSpan

    inParens d = txt "(" <> d <> txt ")"

    -- With UnboxedSums on, @(#@ lexes as one token, so an operator starting
    -- with @#@ cannot sit against its opening bracket.
    spaceOutHash d
      | extensionOn ctx UnboxedSums,
        -- A qualified name never begins with a @#@.
        Unqual (occNameString -> '#' : _) <- x =
          space <> d <> space
      | otherwise = d

-- | A name written with its module.
qualifiedName :: ModuleName -> OccName -> Doc
qualifiedName mname occName = outputable mname <> txt "." <> outputable occName

-- | The name in a module header, with the keyword that introduces it.
moduleHeadName :: Ctx -> ModuleName -> Doc
moduleHeadName ctx mname =
  txt keyword <> space <> outputable mname
  where
    keyword = case ctxSourceType ctx of
      ModuleSource -> "module"
      SignatureSource -> "signature"

-- | A name as it appears in an import or export list.
wrappedName :: Ctx -> IEWrappedName GhcPs -> Doc
wrappedName ctx = \case
  IEName _ x -> name ctx x
  IEDefault _ x -> keyed "default" x
  IEPattern _ x -> keyed "pattern" x
  IEType _ x -> keyed "type" x
  IEData _ x -> keyed "data" x
  where
    keyed kw x = txt kw <> space <> name ctx x

-- | The @type@ or @data@ that disambiguates which namespace is meant.
namespaceSpec :: NamespaceSpecifier -> Doc
namespaceSpec = \case
  NoNamespaceSpecifier -> mempty
  TypeNamespaceSpecifier _ -> txt "type" <> space
  DataNamespaceSpecifier _ -> txt "data" <> space

-- | A multiplicity annotation on an arrow or a field.
multiplicity :: (mult -> Doc) -> HsMultAnnOf mult GhcPs -> Doc
multiplicity render = \case
  HsUnannotated _ -> mempty
  HsLinearAnn _ -> txt "%1"
  HsExplicitMult _ mult -> txt "%" <> render mult

----------------------------------------------------------------------------
-- Definition heads

-- | The left-hand side of a definition: a name and the things it is applied
-- to.
--
-- Written infix, the first two arguments straddle the name and any further
-- ones force the whole of that into parentheses, which is the only way the
-- source could have been written. Written prefix, the arguments simply
-- follow. The indentation flag is for the callers whose body is going to be
-- indented anyway, so that the arguments do not end up two steps in.
defHead ::
  -- | Written infix?
  Bool ->
  -- | Indent the arguments?
  Bool ->
  -- | The name.
  Doc ->
  -- | The arguments.
  [Doc] ->
  Doc
defHead True indentArgs nameDoc (a0 : a1 : rest) =
  wrap (a0 <> breakOrSpace <> indent (align (nameDoc <> space <> a1)))
    <> includeUnless (null rest) (nest (steps indentArgs) (breakOrSpace <> spread rest))
  where
    wrap = if null rest then id else parens
defHead _ indentArgs nameDoc args =
  nameDoc
    <> includeUnless (null args) (nest (steps indentArgs) (breakOrSpace <> spread args))

-- | Arguments, each aligned under itself, one per line when broken.
spread :: [Doc] -> Doc
spread = align . sepBy breakOrSpace . map align

steps :: Bool -> Int
steps b = if b then 1 else 0
