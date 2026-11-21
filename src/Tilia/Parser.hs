{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Turning source text into a syntax tree and a comment stream.
module Tilia.Parser
  ( ParsedModule (..),
    writtenInHeader,
    parseModule,
    parseConfiguration,
    readAlike,
    importLayout,
    codeLines,
    ParseError (..),
    describeParseError,
    ParserConfig (..),
    ghcLibParserVersion,
  )
where

import Data.Choice (fromBool)
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (isSuffixOf, sortOn)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer qualified as GHC
import GHC.Driver.Flags qualified as GHC
import GHC.Hs (AnnsModule (..), EpAnn (anns), HsModule (..), XModulePs (..))
import GHC.Hs.Extension (GhcPs)
import GHC.LanguageExtensions.Type (Extension (Cpp, ForeignFunctionInterface))
import GHC.Parser qualified as GHC
import GHC.Parser.Errors.Types (PsMessage (..))
import GHC.Parser.Lexer qualified as GHC
import GHC.Types.Error qualified as GHC
import GHC.Types.SrcLoc qualified as GHC
import GHC.Unit.Module.Warnings (emptyWarningCategorySet)
import GHC.Utils.Error qualified as GHC
import GHC.Utils.Outputable qualified as GHC

import Tilia.Gathered (Gathered (..), gathered)
import Tilia.Pragma
  ( Defaults,
    effectiveExtensions,
    headerPragmas,
    safeImportsOn,
  )
import Tilia.Source
  ( Lines,
    Source,
    SourceType (..),
    Written (..),
    lineTexts,
    linesOf,
    sourceOf,
  )
import Tilia.Span (Span (..), startPoint)
import Tilia.Span.Ghc (spanOf)

-- | A parsed module, together with the comments found in it.
data ParsedModule = ParsedModule
  { -- | The syntax tree, exactly as GHC produced it.
    pmModule :: HsModule GhcPs,
    -- | The module as its author wrote it.
    pmSource :: Source,
    -- | The nodes of the syntax tree that formatting queries.
    pmGathered :: Gathered,
    -- | Whether GHC read this as a module or as a Backpack signature.
    pmSourceType :: SourceType,
    -- | The lines above the module that the parser never sees.
    --
    -- The lexer skips a @#!@ line, which puts it in no annotation and no
    -- node, so nothing downstream could put it back.
    pmPrologue :: [Text],
    -- | Where the file header stops: at the @module@ line, or where a
    -- module without one begins, if it has anything but a header.
    --
    -- GHC reads pragmas from the header and nowhere else, so this is the
    -- line that decides whether a @{-# … #-}@ is a pragma at all. One
    -- written below it has no effect on compilation, and hoisting it to the
    -- top of the module would change its meaning.
    pmHeaderEnd :: Maybe Span,
    -- | The extensions it was read with.
    pmExtensions :: Set Extension,
    -- | Where each Haddock begins that GHC leaves out of the tree, as
    -- @-Winvalid-haddock@ says.
    pmLooseHaddocks :: Set (Int, Int)
  }

-- | Was something that begins here written in the header, before what
-- 'pmHeaderEnd' marks?
writtenInHeader :: Maybe Span -> Span -> Bool
writtenInHeader headerEnd s =
  maybe True ((startPoint s <) . startPoint) headerEnd

-- | Parse a module.
parseModule ::
  -- | Parser config.
  ParserConfig ->
  -- | Path, reported back in positions, whose extension tells a signature
  -- from a module.
  FilePath ->
  -- | The source.
  Text ->
  Either ParseError ParsedModule
parseModule config path source = parseConfiguration config path ls ls source
  where
    ls = linesOf (Written source)

-- | Parse one configuration of a module.
parseConfiguration ::
  ParserConfig ->
  -- | Path, reported back in positions, whose extension tells a signature
  -- from a module.
  FilePath ->
  -- | The lines of the module as written, except for the lines that do not
  -- belong to this configuration.
  Lines ->
  -- | The lines a Haddock's text is read from.
  Lines ->
  -- | The configuration of it to parse.
  Text ->
  Either ParseError ParsedModule
parseConfiguration config path written haddocks source =
  case GHC.unP entryPoint start of
    GHC.PFailed pstate -> Left (whyNot pstate)
    GHC.POk pstate (GHC.L _ hsModule)
      | not (GHC.isEmptyMessages (GHC.getPsErrorMessages pstate)) ->
          Left (whyNot pstate)
      | otherwise ->
          Right
            ParsedModule
              { pmModule = hsModule,
                pmSource =
                  uncurry (sourceOf readByCpp written haddocks) (lexed start),
                pmGathered = gathered hsModule,
                pmSourceType = sourceType,
                pmPrologue = prologueOf (lineTexts written),
                pmHeaderEnd = headerEndOf hsModule,
                pmExtensions = extensions,
                pmLooseHaddocks = looseHaddocks pstate
              }
  where
    extensions = effectiveExtensions (pcDefaults config) source
    readByCpp = fromBool (Set.member Cpp extensions)
    start = stateFor config extensions path source

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

-- | Return each line among the imports of any branch that a token other
-- than a comment begins on, and whether the leftmost such token on the line
-- begins an import. A declaration written inside a conditional is passed
-- over, since another branch may hold imports after it.
-- 'Nothing' if the module has no imports, writes its top level in explicit
-- braces, or does not lex.
importLayout ::
  -- | Parser config.
  ParserConfig ->
  -- | Is a line written inside a conditional?
  (Int -> Bool) ->
  -- | The module, with its preprocessor directives blanked.
  Text ->
  -- | The import layout.
  Maybe (IntMap Bool)
importLayout config conditional source =
  case GHC.unP (lexing Nothing Nothing False IntMap.empty) start of
    GHC.POk _ found -> found
    GHC.PFailed _ -> Nothing
  where
    start =
      stateFor config (effectiveExtensions (pcDefaults config) source) "" source
    lexing topLevel previous passing found =
      GHC.lexer False $ \(GHC.L s t) -> case GHC.srcSpanStart s of
        _ | GHC.ITeof <- t -> pure (atTopLevel topLevel found)
        GHC.RealSrcLoc l _
          | code t ->
              let line = GHC.srcLocLine l
                  column = GHC.srcLocCol l
                  found' = IntMap.insertWith min line column found
               in case (t, previous, topLevel) of
                    (GHC.ITocurly, Just GHC.ITwhere, Nothing) -> pure Nothing
                    (GHC.ITimport, _, Nothing) ->
                      lexing (Just column) (Just t) False found'
                    (_, _, Nothing) -> lexing topLevel (Just t) False found
                    (_, _, Just c)
                      | column <= c,
                        not (isImport t) ->
                          if conditional line
                            then lexing topLevel (Just t) True found
                            else pure (atTopLevel topLevel found)
                      | passing,
                        column > c ->
                          lexing topLevel (Just t) True found
                    _ -> lexing topLevel (Just t) False found'
        _ -> lexing topLevel previous passing found
    atTopLevel topLevel found = (\c -> fmap (<= c) found) <$> topLevel
    isImport = \case
      GHC.ITimport -> True
      _ -> False
    code = \case
      GHC.ITvocurly -> False
      GHC.ITvccurly -> False
      GHC.ITsemi -> False
      t -> not (isComment t)

-- | The first and the last line of each token of a module that is code:
-- neither a comment, nor one the layout puts in, nor a quasi-quotation,
-- whose text is reproduced as written. None where the module does not lex.
codeLines ::
  -- | Parser config.
  ParserConfig ->
  -- | The module, with its preprocessor directives blanked.
  Text ->
  [(Int, Int)]
codeLines config source =
  case tokensOf s of
    GHC.PFailed _ -> []
    GHC.POk _ tokens ->
      [ (GHC.srcSpanStartLine l, GHC.srcSpanEndLine l)
      | GHC.L (GHC.RealSrcSpan l _) t <- tokens,
        code t
      ]
  where
    s =
      stateFor config (effectiveExtensions (pcDefaults config) source) "" source
    code = \case
      GHC.ITvocurly -> False
      GHC.ITvccurly -> False
      GHC.ITsemi -> False
      t -> not (isComment t || isQuasiQuote t)

-- | Lex every comment, in source order, and the line each quasi-quotation
-- written over several lines ends on, by the line it begins on, from the
-- state a parse starts in.
lexed :: GHC.PState -> ([(GHC.RealSrcSpan, GHC.Token)], IntMap Int)
lexed s =
  case tokensOf s of
    GHC.PFailed _ -> error "Tilia: a module that parses cannot fail to lex"
    GHC.POk _ tokens ->
      ( [ (l, t)
        | GHC.L (GHC.RealSrcSpan l _) t <- tokens,
          isComment t
        ],
        IntMap.fromList
          [ (GHC.srcSpanStartLine l, GHC.srcSpanEndLine l)
          | GHC.xtest GHC.QqBit (GHC.pExtsBitmap (GHC.options s)),
            GHC.L (GHC.RealSrcSpan l _) t <- tokens,
            isQuasiQuote t,
            GHC.srcSpanStartLine l < GHC.srcSpanEndLine l
          ]
      )

-- | Lex a module from the state a parse starts in, with relaxed layout:
-- with no parser to close a layout block at a token that cannot continue
-- it, a brace that opens a block left of a block left open would not lex.
tokensOf ::
  -- | The state a parse of the module starts in.
  GHC.PState ->
  GHC.ParseResult [GHC.Located GHC.Token]
tokensOf s =
  GHC.lexTokenStream
    options
      { GHC.pExtsBitmap =
          GHC.xset GHC.RelaxedLayoutBit (GHC.pExtsBitmap options)
      }
    (GHC.buffer s)
    (GHC.psRealLoc (GHC.loc s))
  where
    options = GHC.options s

-- | Is this token a comment?
isComment :: GHC.Token -> Bool
isComment = \case
  GHC.ITlineComment _ _ -> True
  GHC.ITblockComment _ _ -> True
  GHC.ITdocComment _ _ -> True
  _ -> False

-- | Is this token a quasi-quotation?
isQuasiQuote :: GHC.Token -> Bool
isQuasiQuote = \case
  GHC.ITquasiQuote _ -> True
  GHC.ITqQuasiQuote _ -> True
  _ -> False

-- | The state to start reading a module in.
--
-- Safe imports only make @safe@ a keyword, which it is anyway where
-- @ForeignFunctionInterface@ is in force, so they are asked about only
-- where it is not.
stateFor ::
  -- | Parser config.
  ParserConfig ->
  -- | The extensions to read it with.
  Set Extension ->
  -- | Path, reported back in positions.
  FilePath ->
  -- | The module.
  Text ->
  GHC.PState
stateFor config extensions path source =
  GHC.initParserState
    (parserOpts extensions safe)
    (GHC.stringBufferFromByteString (T.encodeUtf8 source))
    (GHC.mkRealSrcLoc (mkFastString path) 1 1)
  where
    safe =
      Set.notMember ForeignFunctionInterface extensions
        && safeImportsOn (pcDefaults config) source

-- | Would the parser read these two texts with the same extensions?
--
-- Safe imports decide only whether @import safe@ parses, not how anything
-- is printed, so they are left out.
readAlike ::
  -- | Parser config.
  ParserConfig ->
  -- | One text.
  Text ->
  -- | The other.
  Text ->
  Bool
readAlike config a b =
  headerPragmas a == headerPragmas b || bits a == bits b
  where
    bits t =
      GHC.pExtsBitmap
        (parserOpts (effectiveExtensions (pcDefaults config) t) False)

-- | Options to parse with.
parserOpts ::
  -- | The extensions in force.
  Set Extension ->
  -- | Are safe imports on?
  Bool ->
  GHC.ParserOpts
parserOpts extensions safe =
  GHC.mkParserOpts
    (EnumSet.fromList (Set.toList extensions))
    quietDiagnostics
    safe
    True -- keep Haddock tokens
    False -- keep ordinary comment tokens
    True -- let @LINE@ and @COLUMN@ pragmas move the source position

-- | Diagnostics are not reported, so the settings only have to be
-- well-formed, but for @-Winvalid-haddock@, which says where the Haddocks
-- GHC leaves out of the tree are.
quietDiagnostics :: GHC.DiagOpts
quietDiagnostics =
  GHC.DiagOpts
    { GHC.diag_warning_flags = EnumSet.fromList [GHC.Opt_WarnInvalidHaddock],
      GHC.diag_fatal_warning_flags = EnumSet.empty,
      GHC.diag_custom_warning_categories = emptyWarningCategorySet,
      GHC.diag_fatal_custom_warning_categories = emptyWarningCategorySet,
      GHC.diag_warn_is_error = False,
      GHC.diag_reverse_errors = False,
      GHC.diag_max_errors = Nothing,
      GHC.diag_ppr_ctx = GHC.defaultSDocContext
    }

-- | Where each Haddock begins that GHC leaves out of the tree a parse
-- produced.
looseHaddocks ::
  -- | The state the parse ended in.
  GHC.PState ->
  Set (Int, Int)
looseHaddocks pstate =
  Set.fromList
    [ startPoint s
    | m <- toList (GHC.getMessages (fst (GHC.getPsMessages pstate))),
      loose (GHC.errMsgDiagnostic m),
      Just s <- [spanOf (GHC.errMsgSpan m)]
    ]
  where
    loose = \case
      PsWarnHaddockInvalidPos -> True
      PsWarnHaddockIgnoreMulti -> True
      _ -> False

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
  listToMaybe . sortOn startPoint . catMaybes $
    [spanOf (am_mod moduleAnns), spanOf (am_sig moduleAnns)]
      <> fmap spanOf (hsmodImports hsModule)
      <> fmap spanOf (hsmodDecls hsModule)
  where
    moduleAnns = anns (hsmodAnn (hsmodExt hsModule))

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
  { -- | What the component of the module puts in force before its pragmas.
    pcDefaults :: Defaults
  }

-- | What a file's name says it holds.
sourceTypeOf :: FilePath -> SourceType
sourceTypeOf path
  | ".hsig" `isSuffixOf` path = SignatureSource
  | otherwise = ModuleSource

-- | The version of @ghc-lib-parser@ this was built against.
ghcLibParserVersion :: String
ghcLibParserVersion = VERSION_ghc_lib_parser
