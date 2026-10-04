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
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Tilia.Fixity
  ( Brought (..),
    Established (..),
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
    spellDisagreement,
    spellFixity,
    spellUnreadIn,
  )
import Tilia.Palette (Color (Operator, Place), Palette, paint)
import Tilia.Parser (ParsedModule (..))
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
    -- | Where reading it went before giving up, for each way that left some
    -- names unsettled, ending at the module that actually stopped it. Empty
    -- for an import that settles every name; a way is empty for one unread
    -- on its own account.
    noteUnsettled :: [[Text]]
  }
  deriving (Eq, Show)

-- | One operator the module uses.
data OperatorNote = OperatorNote
  { -- | The operator as the module writes it, qualifier and all.
    noteSpelling :: Text,
    -- | What the scope answered for it.
    noteResolution :: Resolution,
    -- | What each import in scope brings, where they disagree about it.
    noteDisagreement :: Maybe (NonEmpty (Text, Fixity))
  }
  deriving (Eq, Show)

-- | Record everything that decided one module's fixities.
fixityNotes ::
  -- | Whether @ImplicitPrelude@ is on, so that the Prelude is listed among
  -- the imports exactly when the module actually has it.
  Choice "implicitPrelude" ->
  -- | What reading each module in scope established, as the resolver
  -- answers it.
  (Text -> IO Established) ->
  -- | The scope the module was formatted under.
  Scope ->
  -- | The module's configurations.
  NonEmpty ParsedModule ->
  IO FixityNotes
fixityNotes implicitPrelude resolve scope configurations = do
  brought <- traverse alongside (moduleImports implicitPrelude (fmap pmModule configurations))
  pure
    FixityNotes
      { notedImports = brought,
        notedOperators = fmap aboutOperator used,
        notedDeclarations = here
      }
  where
    alongside i = do
      answer <- resolve (importModule i)
      let fixities = establishedFixities answer
          readNothing =
            Map.null fixities
              && Set.null (broughtNames (establishedBrought answer))
              && not (Set.null (establishedUntold answer))
      pure
        ImportNote
          { noteModule = importModule i,
            noteAlias =
              if importAlias i == importModule i
                then Nothing
                else Just (importAlias i),
            noteQualified = importQualified i,
            noteBrought =
              if readNothing
                then Nothing
                else Just (Set.size (Set.map snd (Map.keysSet fixities))),
            noteUnsettled =
              Set.toList (Map.keysSet (establishedUnsettled answer) <> establishedUntold answer)
          }

    used =
      Map.elems
        ( Map.fromList
            [ ((namespace, uncurry operatorSpelling u), (namespace, u))
            | (namespace, u) <- concatMap (operatorsUsed . pmGathered) configurations
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
          noteDisagreement =
            Map.lookup (qualifier, op) (reachAmbiguous (reachIn namespace scope))
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
          Nothing -> "could not be read" <> through (noteUnsettled i)
          Just n
            | null (noteUnsettled i) -> operators n
            | otherwise -> operators n <> ", not settling every name" <> through (noteUnsettled i)

    through ways = case filter (not . null) ways of
      [] -> ""
      below -> ", through " <> T.intercalate " or " (fmap (T.intercalate " → " . fmap named) below)

    qualification i = case (noteQualified i, noteAlias i) of
      (True, Just alias) -> " qualified as " <> named alias
      (True, Nothing) -> " qualified"
      (False, Just alias) -> " as " <> named alias
      (False, Nothing) -> ""

    fromOwn (op, fixity) = operator op <> " " <> spellFixity fixity

    fromOperator o =
      operator (noteSpelling o)
        <> " "
        <> case noteResolution o of
          Resolved fixity provenance ->
            spellFixity fixity <> ", " <> from provenance <> ambiguously o
          Unresolved missing ->
            "unknown: may be declared in " <> spellUnreadIn palette missing

    from = \case
      DeclaredHere -> "declared in this module"
      DeclaredIn m -> "declared in " <> named m
      BuiltIn -> "built into the language"
      ReportDefault -> "the Report's default, nothing in scope declaring it"

    ambiguously o = case noteDisagreement o of
      Nothing -> ""
      Just brought ->
        ", and the imports disagree about it: " <> spellDisagreement palette brought

    named = paint palette Place
    operator = paint palette Operator

    operators = \case
      1 -> "1 operator"
      n -> T.pack (show n) <> " operators"
