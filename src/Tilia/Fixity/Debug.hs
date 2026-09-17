{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | An account of how a module's fixities were determined.
module Tilia.Fixity.Debug
  ( FixityNotes (..),
    ImportNote (..),
    OperatorNote (..),
    fixityNotes,
    renderFixityNotes,
  )
where

import Data.Choice (Choice)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs (HsModule)
import GHC.Hs.Extension (GhcPs)
import Tilia.Fixity
  ( Direction (..),
    Fixities,
    Fixity (..),
    Import (..),
    OpName (..),
    Provenance (..),
    Resolution (..),
    Scope (..),
    lookupFixity,
    moduleImports,
    operatorSpelling,
    operatorsUsed,
    reachAmbiguous,
    reachIn,
    reachUnqualified,
    spellUnreadIn,
  )
import Tilia.Palette (Color (Operator, Place), Palette, paint)
import Tilia.Utils (indent, lineWidth, wrapTo)

-- | Everything that decided one module's fixities.
data FixityNotes = FixityNotes
  { -- | What each import brought, in the order the module writes them.
    notedImports :: [ImportNote],
    -- | Resolutions per operator.
    notedOperators :: [OperatorNote],
    -- | What operators the module declares.
    notedDeclarations :: [(Text, Fixity)]
  }
  deriving (Eq, Show)

-- | One import in the module.
data ImportNote = ImportNote
  { -- | The module imported.
    noteModule :: Text,
    -- | The name it goes under here, when that differs from its own.
    noteAlias :: Maybe Text,
    -- | Whether it was imported qualified.
    noteQualified :: Bool,
    -- | How many operators it was read for, or 'Nothing' when it could not
    -- be read at all.
    noteBrought :: Maybe Int,
    -- | Where reading it went before giving up, ending at the module that
    -- actually stopped it. Empty for an import that was read, and for one
    -- unread on its own account.
    noteChain :: [Text]
  }
  deriving (Eq, Show)

-- | One operator the module uses.
data OperatorNote = OperatorNote
  { -- | The operator as the module writes it, qualifier and all.
    noteSpelling :: Text,
    -- | What the scope answered for it.
    noteResolution :: Resolution,
    -- | Whether two modules in scope disagree about it.
    noteAmbiguous :: Bool
  }
  deriving (Eq, Show)

-- | Record everything that decided one module's fixities.
fixityNotes ::
  -- | Whether @ImplicitPrelude@ is on, so that the Prelude is listed among
  -- the imports exactly when the module actually has it.
  Choice "implicitPrelude" ->
  -- | What each module in scope exports, as the resolver answers it.
  (Text -> IO (Maybe (Fixities))) ->
  -- | Where reading a module went before giving up, asked only of the ones
  -- the line above gave up on.
  (Text -> IO [Text]) ->
  -- | The scope the module was formatted under.
  Scope ->
  -- | The module.
  HsModule GhcPs ->
  IO FixityNotes
fixityNotes implicitPrelude resolve chainOf scope hsModule = do
  brought <- traverse alongside (moduleImports implicitPrelude hsModule)
  pure
    FixityNotes
      { notedImports = brought,
        notedOperators = fmap aboutOperator used,
        notedDeclarations = here
      }
  where
    alongside i = do
      answer <- resolve (importModule i)
      below <- case answer of
        Just _ -> pure []
        Nothing -> chainOf (importModule i)
      pure
        ImportNote
          { noteModule = importModule i,
            noteAlias =
              if importAlias i == importModule i
                then Nothing
                else Just (importAlias i),
            noteQualified = importQualified i,
            noteBrought = Set.size . Set.fromList . fmap snd . Map.keys <$> answer,
            noteChain = below
          }

    used =
      Map.elems
        ( Map.fromList
            [ ((namespace, uncurry operatorSpelling u), (namespace, u))
            | (namespace, u) <- operatorsUsed hsModule
            ]
        )

    here =
      [ (op, fixity)
      | (OpName op, (fixity, DeclaredHere)) <- Map.toList declaredHere
      ]
    declaredHere =
      Map.union
        (reachUnqualified (scopeInTerms scope))
        (reachUnqualified (scopeInTypes scope))

    aboutOperator (namespace, (qualifier, op)) =
      OperatorNote
        { noteSpelling = operatorSpelling qualifier op,
          noteResolution = lookupFixity scope namespace qualifier op,
          noteAmbiguous =
            (qualifier, op) `elem` reachAmbiguous (reachIn namespace scope)
        }

-- | Set out all the 'FixityNotes' per file.
renderFixityNotes :: Palette -> Map FilePath FixityNotes -> [Text]
renderFixityNotes palette notes =
  concat
    [ (indent 1 <> "fixities for " <> paint palette Place (T.pack path))
        : aboutFile palette told
    | (path, told) <- Map.toList notes
    ]

-- | One file's account, in reading order.
aboutFile :: Palette -> FixityNotes -> [Text]
aboutFile palette notes =
  concat
    [ section "imports" fromImport (notedImports notes),
      section "operators" fromOperator (notedOperators notes),
      section "declared here" fromOwn (notedDeclarations notes)
    ]
  where
    section what render items
      | null items = []
      | otherwise = heading what : concatMap (entry . render) items
    heading what = indent 2 <> "· " <> what

    entry line = case wrapTo (lineWidth - 8) line of
      [] -> []
      (opening : rest) -> (indent 3 <> "· " <> opening) : fmap (indent 4 <>) rest

    fromImport i =
      named (noteModule i)
        <> qualification i
        <> ": "
        <> case noteBrought i of
          Nothing -> "could not be read" <> through (noteChain i)
          Just n -> operators n

    through = \case
      [] -> ""
      below -> ", through " <> T.intercalate " → " (fmap named below)

    qualification i = case (noteQualified i, noteAlias i) of
      (True, Just alias) -> " qualified as " <> named alias
      (True, Nothing) -> " qualified"
      (False, Just alias) -> " as " <> named alias
      (False, Nothing) -> ""

    fromOwn (op, fixity) = operator op <> " " <> spelled fixity

    fromOperator o =
      operator (noteSpelling o)
        <> " "
        <> case noteResolution o of
          Resolved fixity provenance ->
            spelled fixity <> ", " <> from provenance <> ambiguously o
          Unresolved missing ->
            "unknown: may be declared in " <> spellUnreadIn palette missing

    from = \case
      DeclaredHere -> "declared in this module"
      DeclaredIn m -> "declared in " <> named m
      ReportDefault -> "the Report's default, nothing in scope declaring it"

    ambiguously o
      | noteAmbiguous o = ", and two modules in scope disagree about it"
      | otherwise = ""

    named = paint palette Place
    operator = paint palette Operator

    operators = \case
      1 -> "1 operator"
      n -> T.pack (show n) <> " operators"

-- | A fixity, written the way it would be declared.
spelled :: Fixity -> Text
spelled (Fixity direction precedence) =
  which direction <> " " <> T.pack (show precedence)
  where
    which = \case
      LeftAssoc -> "infixl"
      RightAssoc -> "infixr"
      NoAssoc -> "infix"
