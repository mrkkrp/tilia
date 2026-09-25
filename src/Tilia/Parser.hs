{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Turning source text into a syntax tree and a comment stream.
module Tilia.Parser
  ( ParsedModule (..),
    parseModule,
    parseConfiguration,
    ParseError (..),
    describeParseError,
    ParserConfig (..),
    defaultParserConfig,
    parserConfigFor,
    ghcLibParserVersion,
  )
where

import Data.Foldable (toList)
import Data.List (isSuffixOf, nub, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer qualified as GHC
import GHC.Driver.Session qualified as GHC
import GHC.Hs (HsModule (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension)
import GHC.Parser qualified as GHC
import GHC.Parser.Annotation (getLocA)
import GHC.Parser.Lexer qualified as GHC
import GHC.Types.Error qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
import GHC.Utils.Error qualified as GHC
import GHC.Utils.Outputable qualified as GHC
import Tilia.Pragma (effectiveExtensions)
import Tilia.Source (Lines, Source, SourceType (..), Written (..), lineTexts, linesOf, sourceOf)
import Tilia.Span (Span (..))
import Tilia.Span.Ghc (spanOfReal)

-- | A parsed module, together with the comments found in it.
data ParsedModule = ParsedModule
  { -- | The syntax tree, exactly as GHC produced it.
    pmModule :: HsModule GhcPs,
    -- | The module as its author wrote it.
    pmSource :: Source,
    -- | Whether GHC read this as a module or as a Backpack signature.
    pmSourceType :: SourceType,
    -- | The lines above the module that the parser never sees.
    --
    -- The lexer skips a @#!@ line, which puts it in no annotation and no
    -- node, so nothing downstream could put it back.
    pmPrologue :: [Text],
    -- | Where the file header stops and the module proper begins, if the
    -- module has anything after its header.
    --
    -- GHC reads pragmas from the header and nowhere else, so this is the
    -- line that decides whether a @{-# … #-}@ is a pragma at all. One
    -- written below it has no effect on compilation, and hoisting it to the
    -- top of the module would change its meaning.
    pmHeaderEnd :: Maybe Span
  }

-- | Parse a module.
parseModule ::
  -- | Parser config.
  ParserConfig ->
  -- | Path, used only in positions reported back.
  FilePath ->
  -- | The source.
  Text ->
  Either ParseError ParsedModule
parseModule config path source =
  parseConfiguration config path (linesOf (Written source)) source

-- | Parse one configuration of a module.
parseConfiguration ::
  ParserConfig ->
  -- | Path, used only in positions reported back.
  FilePath ->
  -- | The lines of the module as written, except for the lines that do not
  -- belong to this configuration.
  Lines ->
  -- | The configuration of it to parse.
  Text ->
  Either ParseError ParsedModule
parseConfiguration config path written source =
  case GHC.unP entryPoint initialState of
    GHC.PFailed pstate -> Left (whyNot pstate)
    GHC.POk pstate (GHC.L _ hsModule)
      | not (GHC.isEmptyMessages (GHC.getPsErrorMessages pstate)) ->
          Left (whyNot pstate)
      | otherwise ->
          Right
            ParsedModule
              { pmModule = hsModule,
                pmSource = sourceOf written (headerComments pstate) hsModule,
                pmSourceType = sourceType,
                pmPrologue = prologueOf (lineTexts written),
                pmHeaderEnd = headerEndOf hsModule
              }
  where
    headerComments = concat . GHC.header_comments

    sourceType = sourceTypeOf path

    entryPoint = case sourceType of
      ModuleSource -> GHC.parseModule
      SignatureSource -> GHC.parseSignature

    whyNot pstate =
      case sortOn at (toList (GHC.getMessages (GHC.getPsErrorMessages pstate))) of
        m : _ -> ParseError{peSpan = GHC.errMsgSpan m, peProblem = saying m}
        [] ->
          ParseError
            { peSpan = GHC.mkSrcSpanPs (GHC.last_loc pstate),
              peProblem = "parse error"
            }

    at m = case GHC.srcSpanToRealSrcSpan (GHC.errMsgSpan m) of
      Just s -> (GHC.srcSpanStartLine s, GHC.srcSpanStartCol s)
      Nothing -> (maxBound, maxBound)

    saying =
      T.pack
        . GHC.showSDocUnsafe
        . GHC.vcat
        . GHC.unDecorated
        . GHC.diagnosticMessage GHC.NoDiagnosticOpts
        . GHC.errMsgDiagnostic

    config' =
      config
        { pcExtensions = withImplied (effectiveExtensions (pcExtensions config) source)
        }

    initialState =
      GHC.initParserState
        (parserOpts config')
        (GHC.stringToStringBuffer (T.unpack source))
        (GHC.mkRealSrcLoc (mkFastString path) 1 1)

-- | Close a set of extensions under what they imply.
withImplied :: [Extension] -> [Extension]
withImplied = settle . nub
  where
    settle es =
      let es' = nub (es <> concatMap implied es)
       in if length es' == length es then es else settle es'
    implied e = [to | (from, GHC.On to) <- GHC.impliedXFlags, from == e]

-- | Options to parse with.
parserOpts :: ParserConfig -> GHC.ParserOpts
parserOpts ParserConfig{pcExtensions} =
  GHC.mkParserOpts
    (EnumSet.fromList pcExtensions)
    quietDiagnostics
    False -- safe imports
    True -- keep Haddock tokens
    True -- keep ordinary comment tokens
    True -- let @LINE@ and @COLUMN@ pragmas move the source position

-- | Diagnostics are not reported, so the settings only have to be
-- well-formed.
quietDiagnostics :: GHC.DiagOpts
quietDiagnostics =
  GHC.DiagOpts
    { GHC.diag_warning_flags = EnumSet.empty,
      GHC.diag_fatal_warning_flags = EnumSet.empty,
      GHC.diag_custom_warning_categories = emptyWarningCategorySet,
      GHC.diag_fatal_custom_warning_categories = emptyWarningCategorySet,
      GHC.diag_warn_is_error = False,
      GHC.diag_reverse_errors = False,
      GHC.diag_max_errors = Nothing,
      GHC.diag_ppr_ctx = GHC.defaultSDocContext
    }

-- | The @#!@ lines a file begins with, and the empty line after them.
prologueOf :: [Text] -> [Text]
prologueOf ls = case span isShebang ls of
  ([], _) -> []
  (shebangs, rest) -> shebangs <> filter T.null (take 1 rest)
  where
    isShebang = T.isPrefixOf "#!"

-- | The start of the first thing that is not part of the header.
headerEndOf :: HsModule GhcPs -> Maybe Span
headerEndOf hsModule =
  foldl' earliest Nothing $
    fmap getLocA (hsmodImports hsModule)
      <> fmap getLocA (hsmodDecls hsModule)
  where
    earliest acc l = case GHC.srcSpanToRealSrcSpan l of
      Nothing -> acc
      Just s ->
        let this = spanOfReal s
         in Just (maybe this (keepEarlier this) acc)
    keepEarlier a b
      | (spanStartLine a, spanStartColumn a) <= (spanStartLine b, spanStartColumn b) = a
      | otherwise = b

-- | Why a module did not parse.
data ParseError = ParseError
  { -- | Where the parser gave up.
    peSpan :: GHC.SrcSpan,
    -- | GHC's rendered error message.
    peProblem :: Text
  }

-- | Present 'ParseError' in a human-friendly form.
describeParseError :: ParseError -> Text
describeParseError e =
  T.pack (GHC.showSDocUnsafe (GHC.ppr (peSpan e))) <> ": " <> peProblem e

-- | Parser configuration.
newtype ParserConfig = ParserConfig
  { -- | Extensions to enable before parsing.
    pcExtensions :: [Extension]
  }

-- | What to parse with when there is no package to ask.
defaultParserConfig :: ParserConfig
defaultParserConfig = parserConfigFor []

-- | What to parse with, given whatever the package had to say.
parserConfigFor ::
  -- | What the package puts in force, or nothing if there is no package.
  [Extension] ->
  -- | The resulting parser config.
  ParserConfig
parserConfigFor package =
  ParserConfig
    { pcExtensions =
        if null package
          then GHC.languageExtensions (Just GHC.GHC2021)
          else package
    }

-- | What a file's name says it holds.
sourceTypeOf :: FilePath -> SourceType
sourceTypeOf path
  | ".hsig" `isSuffixOf` path = SignatureSource
  | otherwise = ModuleSource

-- | The version of @ghc-lib-parser@ this was built against.
ghcLibParserVersion :: String
ghcLibParserVersion = VERSION_ghc_lib_parser
