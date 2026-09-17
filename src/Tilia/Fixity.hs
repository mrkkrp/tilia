{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Working out the fixity of the operators a module uses.
module Tilia.Fixity
  ( -- * Fixities
    OpName (..),
    Direction (..),
    Fixity (..),
    defaultFixity,

    -- * What a module declares
    declaredFixities,
    declaredNames,
    moduleName,

    -- * What a module passes on
    ExportItem (..),
    moduleExports,
    exportedOperators,
    declaredChildren,
    moduleChildren,

    -- * What a module can see
    Import (..),
    ImportItem (..),
    moduleImports,
    mightBring,
    surelyNames,
    Known (..),
    nothingKnown,
    Namespace (..),
    Fixities,
    inBothNamespaces,
    Unread (..),
    ModuleChain (..),
    spellModuleChain,
    Scope (..),
    Reach (..),
    reachIn,
    resolveScope,

    -- * Answers
    Provenance (..),
    Resolution (..),
    lookupFixity,

    -- * What could not be answered
    Unknown (..),
    operatorsUsed,
    unknownOperators,
    operatorSpelling,
    spellUnreadIn,

    -- * What reading a module established
    Established (..),
    Exported (..),
    exportedNames,
    asExported,
  )
where

import Data.Choice (Choice, isTrue)
import Data.Foldable (toList)
import Data.Generics.Schemes (listify)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Hs hiding (Fixity, OpName)
import GHC.Types.Fixity qualified as GHC
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Palette (Color (Place), Palette, paint)

----------------------------------------------------------------------------
-- Fixities

-- | An operator, spelled as it appears in an @infix@ declaration: @<+>@, or
-- @div@ for a function used infix in backticks.
newtype OpName = OpName Text
  deriving (Eq, Ord, Show)

-- | Which way an operator associates.
data Direction = LeftAssoc | RightAssoc | NoAssoc
  deriving (Eq, Show)

-- | A fixity: how tightly an operator binds, and which way it associates.
data Fixity = Fixity
  { fixityDirection :: Direction,
    fixityPrecedence :: Int
  }
  deriving (Eq, Show)

-- | What an operator with no declaration in scope means: @infixl 9@.
defaultFixity :: Fixity
defaultFixity = Fixity LeftAssoc 9

----------------------------------------------------------------------------
-- What a module declares

-- | Which of Haskell's two namespaces an operator is written in.
data Namespace = InTypes | InTerms
  deriving (Eq, Ord, Show)

-- | The fixities a module offers, by the namespace each is written in.
type Fixities = Map (Namespace, OpName) Fixity

-- | Take fixities that say nothing about namespaces to govern both.
inBothNamespaces :: Map OpName Fixity -> Fixities
inBothNamespaces declared =
  Map.fromList
    [ ((namespace, op), fixity)
    | (op, fixity) <- Map.toList declared,
      namespace <- [InTypes, InTerms]
    ]

-- | The fixities in one namespace, by the operator alone.
fixitiesIn :: Namespace -> Fixities -> Map OpName Fixity
fixitiesIn namespace declared =
  Map.fromList [(op, fixity) | ((n, op), fixity) <- Map.toList declared, n == namespace]

-- | The fixities a module declares for its own operators.
declaredFixities :: HsModule GhcPs -> Fixities
declaredFixities hsModule =
  Map.fromList
    [ ((namespace, op), fixity)
    | (specifier, op, fixity) <- concatMap (fromDecl . unLoc) (hsmodDecls hsModule),
      namespace <- namespacesOf specifier op
    ]
  where
    (types, terms) = declaredNamespaces hsModule
    namespacesOf specifier op = case specifier of
      TypeNamespaceSpecifier _ -> [InTypes]
      DataNamespaceSpecifier _ -> [InTerms]
      NoNamespaceSpecifier ->
        case ([InTypes | Set.member op types] <> [InTerms | Set.member op terms]) of
          [] -> [InTypes, InTerms]
          found -> found

    fromDecl = \case
      SigD _ sig -> fromSig sig
      TyClD _ ClassDecl {tcdSigs} -> concatMap (fromSig . unLoc) tcdSigs
      _ -> []
    fromSig = \case
      FixSig _ (FixitySig specifier names fixity) ->
        [(specifier, opName (unLoc n), fromGhcFixity fixity) | n <- names]
      _ -> []

-- | The names a module declares among types, and those it declares among
-- terms.
declaredNamespaces :: HsModule GhcPs -> (Set OpName, Set OpName)
declaredNamespaces hsModule =
  ( Set.fromList (concatMap (types . unLoc) decls),
    Set.fromList (concatMap (terms . unLoc) decls)
  )
  where
    decls = hsmodDecls hsModule
    types = \case
      TyClD _ d -> case d of
        FamDecl _ FamilyDecl {fdLName} -> [opName (unLoc fdLName)]
        SynDecl {tcdLName} -> [opName (unLoc tcdLName)]
        DataDecl {tcdLName} -> [opName (unLoc tcdLName)]
        ClassDecl {tcdLName, tcdATs} ->
          opName (unLoc tcdLName)
            : [opName (unLoc (fdLName (unLoc f))) | f <- tcdATs]
      _ -> []
    terms = \case
      ValD _ b -> boundNames b
      SigD _ sig -> signedNames sig
      ForD _ f -> [opName (unLoc (fd_name f))]
      TyClD _ d@DataDecl {} -> membersOf d
      TyClD _ ClassDecl {tcdSigs} -> concatMap (classMethods . unLoc) tcdSigs
      _ -> []

-- | Every name a module defines itself.
--
-- Not the same question as 'declaredFixities', which is about @infix@
-- declarations. This one is asked of an export list: a name a module
-- exports and also defines needs no chasing, and one it merely passes on
-- does. Getting the two confused makes a module appear to re-export
-- everything it exports, and then a single dependency whose source is
-- missing makes the whole module unanswerable.
--
-- Erring towards too few is safe and towards too many is not: a name left
-- out here is chased when it need not have been, whereas one wrongly
-- included is a fixity nobody looked for.
declaredNames :: HsModule GhcPs -> Set OpName
declaredNames = Set.fromList . concatMap (fromDecl . unLoc) . hsmodDecls
  where
    fromDecl = \case
      ValD _ b -> fromBind b
      SigD _ sig -> fromSig sig
      TyClD _ t -> fromTyCl t
      ForD _ f -> [opName (unLoc (fd_name f))]
      _ -> []

    fromBind = boundNames

    fromSig = \case
      FixSig _ (FixitySig _ ns _) -> map (opName . unLoc) ns
      sig -> signedNames sig

    fromTyCl = \case
      FamDecl _ (FamilyDecl {fdLName}) -> [opName (unLoc fdLName)]
      SynDecl {tcdLName} -> [opName (unLoc tcdLName)]
      d@DataDecl {tcdLName} -> opName (unLoc tcdLName) : membersOf d
      d@ClassDecl {tcdLName} -> opName (unLoc tcdLName) : membersOf d

-- | The names a binding brings into being.
boundNames :: HsBind GhcPs -> [OpName]
boundNames = \case
  FunBind _ n _ -> [opName (unLoc n)]
  PatBind _ p _ _ -> [opName n | VarPat _ (L _ n) <- listify isVarPat p]
  PatSynBind _ (PSB _ n _ _ _) -> [opName (unLoc n)]
  _ -> []
  where
    isVarPat :: Pat GhcPs -> Bool
    isVarPat = \case
      VarPat {} -> True
      _ -> False

-- | The names a signature is about, leaving fixity declarations aside.
signedNames :: Sig GhcPs -> [OpName]
signedNames = \case
  TypeSig _ ns _ -> map (opName . unLoc) ns
  ClassOpSig _ _ ns _ -> map (opName . unLoc) ns
  PatSynSig _ ns _ -> map (opName . unLoc) ns
  _ -> []

-- | The methods a class signature declares.
classMethods :: Sig GhcPs -> [OpName]
classMethods = \case
  TypeSig _ ns _ -> map (opName . unLoc) ns
  ClassOpSig _ _ ns _ -> map (opName . unLoc) ns
  _ -> []

-- | The names a declaration carries under the name it declares: a data
-- type's constructors and record fields, a class's methods and the
-- families it keeps.
--
-- These are what @T(..)@ stands for, and each of them can carry a fixity of
-- its own—@:|@ is a constructor and @infixr 5@ all the same.
membersOf :: TyClDecl GhcPs -> [OpName]
membersOf = \case
  DataDecl {tcdDataDefn} -> concatMap (fromCon . unLoc) (consOf (dd_cons tcdDataDefn))
  ClassDecl {tcdSigs, tcdATs} ->
    concatMap (classMethods . unLoc) tcdSigs
      <> [opName (unLoc (fdLName (unLoc f))) | f <- tcdATs]
  _ -> []
  where
    consOf :: DataDefnCons (LConDecl GhcPs) -> [LConDecl GhcPs]
    consOf = toList

    fromCon :: ConDecl GhcPs -> [OpName]
    fromCon = \case
      ConDeclGADT {con_names} -> map (opName . unLoc) (toList con_names)
      ConDeclH98 {con_name, con_args} ->
        opName (unLoc con_name) : fieldNames con_args

    -- A record field is a name the type carries too, and it may be an
    -- operator.
    fieldNames :: HsConDeclH98Details GhcPs -> [OpName]
    fieldNames = \case
      RecCon fields ->
        [ opName (unLoc (foLabel (unLoc n)))
        | f <- unLoc fields,
          n <- cdrf_names (unLoc f)
        ]
      _ -> []

-- | What each type or class a module declares carries with it.
--
-- What @T(..)@ stands for where the module declares @T@ itself. Where it
-- does not—a type it merely passes on—there is nothing here, and a caller
-- that finds nothing must not conclude that @T@ brings nothing.
declaredChildren :: HsModule GhcPs -> Map OpName (Set OpName)
declaredChildren =
  Map.fromListWith Set.union . concatMap (fromDecl . unLoc) . hsmodDecls
  where
    fromDecl = \case
      TyClD _ d@DataDecl {tcdLName} -> [entry tcdLName d]
      TyClD _ d@ClassDecl {tcdLName} -> [entry tcdLName d]
      _ -> []
    entry name d = (opName (unLoc name), Set.fromList (membersOf d))

-- | What a module offers under each name, as its export list offers it.
--
-- @T(..)@ in the list hands on everything the module has under @T@; @T(A,
-- B)@ hands on only what it names; no export list at all hands on every
-- member of everything the module declares. This is the answer to \"what
-- does @T(..)@ bring in\" asked of the module being imported from, which is
-- the only place the answer is.
moduleChildren :: HsModule GhcPs -> Map OpName (Set OpName)
moduleChildren hsModule = case hsmodExports hsModule of
  Nothing -> declared
  Just items -> Map.fromListWith Set.union (concatMap (fromIE . unLoc) (unLoc items))
  where
    declared = declaredChildren hsModule
    fromIE = \case
      IEThingAll _ n _ ->
        [(nameOf n, Map.findWithDefault Set.empty (nameOf n) declared)]
      IEThingWith _ n _ ns _ -> [(nameOf n, Set.fromList (map nameOf ns))]
      _ -> []
    nameOf = opName . ieWrappedName . unLoc

-- | Render a parsed name as an operator name.
opName :: RdrName -> OpName
opName = OpName . T.pack . occNameString . rdrNameOcc

fromGhcFixity :: GHC.Fixity -> Fixity
fromGhcFixity (GHC.Fixity prec dir) = Fixity (fromGhcDirection dir) prec

fromGhcDirection :: GHC.FixityDirection -> Direction
fromGhcDirection = \case
  GHC.InfixL -> LeftAssoc
  GHC.InfixR -> RightAssoc
  GHC.InfixN -> NoAssoc

-- | The module's own name, if it declares one.
moduleName :: HsModule GhcPs -> Maybe Text
moduleName = fmap (T.pack . moduleNameString . unLoc) . hsmodName

----------------------------------------------------------------------------
-- What a module passes on

-- | One entry of a module's export list.
data ExportItem
  = -- | A name, which may or may not be declared in this module, under the
    -- qualifier it was written with if it was written with one.
    ExportName (Maybe Text) OpName
  | -- | @T(..)@: the name, and with it whatever the module has to give
    -- under that name. Which names those are cannot be read off the list;
    -- it takes the declaration of @T@, or the module @T@ came from.
    ExportAll (Maybe Text) OpName
  | -- | @module M@, re-exporting everything that module brought in.
    ExportModule Text
  deriving (Eq, Show)

-- | The operators a module's export list names, where that list can be
-- enumerated without reading what the module passes on.
--
-- 'Nothing' is a module that keeps its own counsel: one whose export list
-- hands whole modules on, so that what it exports cannot be known without
-- reading them. A module with no export list at all exports what it
-- declares, and the fixities it declares are everything it could supply.
--
-- What this is for: an operator nobody could settle is blamed on the
-- imports that might have declared it, and a module that plainly exports no
-- such name is not one of them. See 'unreadFor'.
exportedOperators :: HsModule GhcPs -> Maybe (Set OpName)
exportedOperators hsModule = case moduleExports hsModule of
  Nothing -> Just (Set.fromList [op | (_, op) <- Map.keys (declaredFixities hsModule)])
  Just items
    | any beyondUs items -> Nothing
    | otherwise -> Just (Set.unions (map named items))
  where
    declared = declaredChildren hsModule
    beyondUs = \case
      ExportModule _ -> True
      ExportAll _ parent -> not (Map.member parent declared)
      ExportName _ _ -> False
    named = \case
      ExportName _ op -> Set.singleton op
      ExportAll _ parent ->
        Set.insert parent (Map.findWithDefault Set.empty parent declared)
      ExportModule _ -> Set.empty

-- | The qualifier a name was written under.
qualifierOf :: RdrName -> Maybe Text
qualifierOf = \case
  Qual m _ -> Just (T.pack (moduleNameString m))
  _ -> Nothing

-- | A module's export list, or 'Nothing' if it has none.
--
-- The distinction matters. A module with no export list exports exactly
-- what it defines, so its own declarations are the whole answer. A module
-- with one may be passing on names it never declared, and those are what
-- re-export resolution has to chase.
moduleExports :: HsModule GhcPs -> Maybe [ExportItem]
moduleExports =
  fmap (concatMap (fromIE . unLoc) . unLoc) . hsmodExports
  where
    fromIE = \case
      IEVar _ n _ -> [named n]
      IEThingAbs _ n _ -> [named n]
      IEThingAll _ n _ -> [as ExportAll n]
      -- The type itself and every member listed with it; a class exports
      -- its operators this way.
      IEThingWith _ n _ ns _ -> named n : map named ns
      IEModuleContents _ m -> [ExportModule (T.pack (moduleNameString (unLoc m)))]
      _ -> []
    named = as ExportName
    as item n =
      let rdr = ieWrappedName (unLoc n)
       in item (qualifierOf rdr) (opName rdr)

----------------------------------------------------------------------------
-- What a module can see

-- | One import declaration, reduced to what bears on fixity.
data Import = Import
  { -- | The module being imported.
    importModule :: Text,
    -- | Whether unqualified names are brought into scope. An import that is
    -- @qualified@ brings none.
    importQualified :: Bool,
    -- | The name qualified uses go through: the alias if there is one,
    -- otherwise the module's own name.
    importAlias :: Text,
    -- | The explicit list, if there is one, and whether it is a @hiding@
    -- list.
    --
    -- Kept as written rather than as a set of names, because @T(..)@ says
    -- what it brings in only once the module it comes from has been asked.
    importNames :: Maybe (Bool, [ImportItem])
  }
  deriving (Eq, Show)

-- | One entry of an import list.
data ImportItem
  = -- | A plain name.
    ImportedName OpName
  | -- | @T(..)@: the name, and everything the module offers under it.
    ImportedAll OpName
  | -- | @T(a, b)@: the name and the members written out beside it.
    ImportedSome OpName [OpName]
  deriving (Eq, Show)

-- | Could this list bring the operator in?
--
-- Told what the module keeps under each of its names, this is exact. Told
-- nothing about a @T(..)@'s @T@, it answers yes, because ruling the
-- operator out would mean knowing what @T@ has under it and we do not. Used
-- where being wrong the other way—deciding an operator could not have
-- arrived through a list that in fact brings it—would settle a fixity that
-- was never established.
mightBring :: Map OpName (Set OpName) -> OpName -> [ImportItem] -> Bool
mightBring carries op = any $ \case
  ImportedName n -> n == op
  ImportedSome parent ns -> parent == op || op `elem` ns
  ImportedAll parent -> maybe True (names parent) (Map.lookup parent carries)
  where
    names parent kids = parent == op || Set.member op kids

-- | Does this list certainly name the operator?
--
-- The other side of 'mightBring', for a @hiding@ list: a name is hidden
-- only where the list says so outright. Told what a @T(..)@ carries this
-- is again exact; told nothing, it still holds that @T(..)@ hides @T@.
surelyNames :: Map OpName (Set OpName) -> OpName -> [ImportItem] -> Bool
surelyNames carries op = any $ \case
  ImportedName n -> n == op
  ImportedSome parent ns -> parent == op || op `elem` ns
  ImportedAll parent -> maybe (parent == op) (names parent) (Map.lookup parent carries)
  where
    names parent kids = parent == op || Set.member op kids

-- | The imports of a module.
moduleImports ::
  -- | Whether @ImplicitPrelude@ is on.
  Choice "implicitPrelude" ->
  HsModule GhcPs ->
  [Import]
moduleImports implicitPrelude hsModule = prelude <> written
  where
    written = map (fromDecl . unLoc) (hsmodImports hsModule)
    prelude
      | not (isTrue implicitPrelude) = []
      | any ((== "Prelude") . importModule) written = []
      | otherwise =
          [ Import
              { importModule = "Prelude",
                importQualified = False,
                importAlias = "Prelude",
                importNames = Nothing
              }
          ]

    fromDecl d =
      Import
        { importModule = modName (unLoc (ideclName d)),
          importQualified = ideclQualified d /= NotQualified,
          importAlias = maybe (modName (unLoc (ideclName d))) (modName . unLoc) (ideclAs d),
          importNames = fromList <$> ideclImportList d
        }
    fromList (interpretation, names) =
      ( interpretation == EverythingBut,
        mapMaybe (importedItem . unLoc) (unLoc names)
      )
    modName = T.pack . moduleNameString

-- | One entry of an import list, as written.
importedItem :: IE GhcPs -> Maybe ImportItem
importedItem = \case
  IEVar _ n _ -> Just (ImportedName (nameOf n))
  IEThingAbs _ n _ -> Just (ImportedName (nameOf n))
  IEThingAll _ n _ -> Just (ImportedAll (nameOf n))
  IEThingWith _ n _ ns _ -> Just (ImportedSome (nameOf n) (map nameOf ns))
  _ -> Nothing
  where
    nameOf :: LIEWrappedName GhcPs -> OpName
    nameOf = opName . ieWrappedName . unLoc

-- | An import whose module could not be read, and what is known about it
-- regardless.
--
-- Unread is not the same as unknown. Failing to establish a module's
-- fixities does not stop us reading its export list or its declarations,
-- and either can rule the module out as the source of an operator. Ruling
-- it out is what keeps one unreachable package from unsettling a whole
-- file.
data Unread = Unread
  { -- | The import as written.
    unreadImport :: Import,
    -- | The operators its export list names, where that list can be
    -- enumerated. 'Nothing' is a module that keeps its own counsel—one
    -- whose list passes whole modules on, or that could not be parsed—and
    -- which therefore has to be suspected of everything.
    unreadExports :: Maybe (Set OpName),
    -- | What it keeps under each of its names, for expanding a @T(..)@ in
    -- the import list. Empty is ignorance, and leaves such a list
    -- suspected of bringing in anything.
    unreadCarries :: Map OpName (Set OpName),
    -- | The modules below this one that reading went through, ending at
    -- the one that actually stopped it. Empty where the import is itself
    -- what could not be read. Diagnostic only.
    unreadBelow :: [Text]
  }
  deriving (Eq, Show)

-- | An import that could not be read, and the way down to the module that
-- actually stopped us. The head is the import as the file being formatted
-- writes it, and the last name is where reading gave up.
newtype ModuleChain = ModuleChain (NonEmpty Text)
  deriving (Eq, Show)

-- | A chain as it is shown, with the modules painted and arrows between
-- them.
spellModuleChain :: Palette -> ModuleChain -> Text
spellModuleChain palette (ModuleChain modules) =
  T.intercalate " → " (map (paint palette Place) (toList modules))

-- | Every fixity a module can see, and how.
data Scope = Scope
  { -- | What is in scope for an operator written among types.
    scopeInTypes :: Reach,
    -- | What is in scope for one written among terms.
    scopeInTerms :: Reach,
    -- | The imports whose modules could not be read, and what is
    -- nonetheless known about each.
    --
    -- These are what separate \"no declaration exists\" from \"we did not
    -- manage to look\". An operator that was not found is settled only if no
    -- unread import could have brought it in, and deciding that needs the
    -- whole import rather than the module's name: see 'unreadFor'.
    --
    -- One list for both namespaces: a module that could not be read could
    -- not be read for either.
    scopeUnread :: [Unread]
  }
  deriving (Eq, Show)

-- | What one namespace of a scope holds.
data Reach = Reach
  { -- | Reachable without qualification, with where it came from.
    reachUnqualified :: Map OpName (Fixity, Provenance),
    -- | Reachable as @M.op@, keyed by the alias actually written—or by the
    -- module's own name, under which its own declarations are reachable.
    reachQualified :: Map (Text, OpName) (Fixity, Provenance),
    -- | Operators the imports bring in with two different fixities, as they
    -- would have to be written to run into it: without a qualifier, or under
    -- the alias the disagreeing imports share.
    reachAmbiguous :: [(Maybe Text, OpName)]
  }
  deriving (Eq, Show)

-- | The half of a scope an operator written in this namespace is settled
-- against.
reachIn :: Namespace -> Scope -> Reach
reachIn = \case
  InTypes -> scopeInTypes
  InTerms -> scopeInTerms

-- | What is known about the modules a module imports.
--
-- Everything 'resolveScope' cannot read off the module in front of it,
-- gathered into one place. 'nothingKnown' answers none of them, which is
-- legitimate—it costs coverage, never correctness.
data Known = Known
  { -- | What a module exports, or 'Nothing' if that could not be
    -- determined. 'Nothing' means the module could not be read, which is
    -- not the same as its exporting nothing; see 'resolveScope'.
    knownFixities :: Text -> Maybe Fixities,
    -- | What a module keeps under each of its names, so that a @T(..)@ in
    -- an import list can be told what it brings in. An empty map is
    -- ignorance as much as it is emptiness, and understates a list rather
    -- than overstating it.
    knownChildren :: Text -> Map OpName (Set OpName),
    -- | The operators a module's export list names, where that list can be
    -- enumerated without reading what it passes on. Asked only about
    -- modules 'knownFixities' could not answer for, and only to decide
    -- which of them an unsettled operator can be blamed on.
    knownExportNames :: Text -> Maybe (Set OpName),
    -- | The modules reading a module went through before giving up, the one
    -- it gave up on last. Asked only about modules 'knownFixities' could
    -- not answer for, and only so that a message can name the module that
    -- is really in the way.
    knownChain :: Text -> [Text]
  }

-- | Knowing nothing about anything: every question answered with a shrug.
--
-- A scope built on this settles what the module itself declares and
-- nothing more. Fill in the fields that can be answered.
nothingKnown :: Known
nothingKnown =
  Known
    { knownFixities = const Nothing,
      knownChildren = const Map.empty,
      knownExportNames = const Nothing,
      knownChain = const []
    }

-- | Work out what a module can see.
--
-- The lookup function supplies what each imported module exports, and
-- 'Nothing' means it could not be determined—the package was not
-- downloaded, the source did not parse. That distinction is the whole point
-- of its type: an empty map is a fact about a module, whereas 'Nothing' is
-- an admission about us, and conflating them is how a formatter ends up
-- asserting a fixity it never established.
--
-- Not handled here: operators arriving through @T(..)@. That is syntactic
-- and so belongs to the lookup function, as re-export chains do—and those
-- "Tilia.Fixity.Plan" already follows, through export lists in source and
-- through the export section of an interface.
resolveScope ::
  -- | Whether @ImplicitPrelude@ is on.
  Choice "implicitPrelude" ->
  -- | What is known about the modules this one imports.
  Known ->
  HsModule GhcPs ->
  Scope
resolveScope implicitPrelude known hsModule =
  Scope
    { scopeInTypes = reachAmong InTypes,
      scopeInTerms = reachAmong InTerms,
      scopeUnread = unread
    }
  where
    Known {knownFixities = exportsOf, knownChildren, knownExportNames, knownChain} = known
    exportNamesOf = knownExportNames
    imports = moduleImports implicitPrelude hsModule
    declared = declaredFixities hsModule

    reachAmong namespace =
      Reach
        { reachUnqualified = Map.union own (Map.map fst unqualified),
          reachQualified = qualified,
          reachAmbiguous =
            [(Nothing, op) | op <- Map.keys (Map.filter snd unqualified)]
              <> [(Just alias, op) | (alias, op) <- Map.keys (Map.filter snd qualifiedFrom)]
        }
      where
        own = Map.map (,DeclaredHere) (fixitiesIn namespace declared)
        offered m = fixitiesIn namespace <$> exportsOf m
        unqualified =
          Map.unionsWith
            disagree
            [ Map.map (,False) (visible offered i)
            | i <- imports,
              not (importQualified i)
            ]
        qualified = Map.union ownQualified (Map.map fst qualifiedFrom)
        ownQualified =
          Map.fromList
            [ ((m, op), entry)
            | m <- toList (moduleName hsModule),
              (op, entry) <- Map.toList own
            ]
        qualifiedFrom =
          Map.unionsWith
            disagree
            [ Map.mapKeys (importAlias i,) (Map.map (,False) (visible offered i))
            | i <- imports
            ]

    unread =
      [ Unread
          { unreadImport = i,
            unreadExports = exportNamesOf (importModule i),
            unreadCarries = knownChildren (importModule i),
            unreadBelow = knownChain (importModule i)
          }
      | i <- imports,
        Nothing <- [exportsOf (importModule i)]
      ]

    -- Paired with a flag saying whether two imports disagreed about it.
    disagree (a, aBad) (b, bBad) = (a, aBad || bBad || fst a /= fst b)

    visible offered i =
      let exported =
            Map.map (,DeclaredIn (importModule i)) $
              fromMaybe Map.empty (offered (importModule i))
          carries = knownChildren (importModule i)
       in case importNames i of
            Nothing -> exported
            Just (True, hidden) ->
              Map.filterWithKey
                (\op _ -> not (surelyNames carries op hidden))
                exported
            Just (False, shown) ->
              Map.filterWithKey (\op _ -> mightBring carries op shown) exported

----------------------------------------------------------------------------
-- Answers

-- | Where a fixity came from.
--
-- Kept so that an answer can be explained, and so that
-- 'ReportDefault'—which is a real answer, not a guess—cannot be confused
-- with not having one.
data Provenance
  = -- | An @infix@ declaration in the module being formatted.
    DeclaredHere
  | -- | An @infix@ declaration in the named imported module.
    DeclaredIn Text
  | -- | No declaration exists anywhere in scope, and every module in scope
    -- was successfully consulted, so the Report's @infixl 9@ applies.
    ReportDefault
  deriving (Eq, Show)

-- | What is known about an operator at a use site.
data Resolution
  = -- | Established, and here is where from.
    Resolved Fixity Provenance
  | -- | Not established. Each chain is an import that could not be read,
    -- down to the module that actually stopped us, and the answer may be in
    -- any of them.
    --
    -- A printer that receives this must not restructure the operator chain:
    -- it has to lay it out as the input had it. Rearranging on a guess is
    -- exactly what this type exists to prevent.
    Unresolved (NonEmpty ModuleChain)
  deriving (Eq, Show)

-- | The fixity of an operator as this module sees it.
lookupFixity ::
  -- | The scope.
  Scope ->
  -- | The namespace the operator is written in.
  Namespace ->
  -- | The qualifier written at the use site, if any.
  Maybe Text ->
  -- | Operator to resolve.
  OpName ->
  -- | The resolution.
  Resolution
lookupFixity scope namespace qualifier op =
  case settledFor scope namespace qualifier op of
    Just (_, (fixity, provenance)) -> Resolved fixity provenance
    Nothing -> case nonEmpty (unreadFor scope qualifier op) of
      Nothing -> Resolved defaultFixity ReportDefault
      Just missing -> Unresolved missing

-- | What settles a use, and the namespace that settled it.
settledFor ::
  Scope ->
  Namespace ->
  Maybe Text ->
  OpName ->
  Maybe (Namespace, (Fixity, Provenance))
settledFor scope namespace qualifier op =
  case mapMaybe found (namespace : promotedFrom namespace) of
    (answer : _) -> Just answer
    [] -> Nothing
  where
    promotedFrom = \case
      InTypes -> [InTerms]
      InTerms -> []
    found n =
      (n,) <$> case qualifier of
        Nothing -> Map.lookup op (reachUnqualified (reachIn n scope))
        Just q -> Map.lookup (q, op) (reachQualified (reachIn n scope))

-- | The modules of the unread imports that could have settled this use.
--
-- Empty means an operator that was not found really is undeclared, rather
-- than declared somewhere we failed to look. Getting this narrow matters:
-- an import that is @qualified as M@ has no bearing on an operator written
-- without a qualifier, and one with an import list has none on an operator
-- the list does not name. Were every unread import to count against every
-- operator, one unreachable package deep in a dependency tree would
-- unsettle a whole file.
unreadFor ::
  -- | The scope.
  Scope ->
  -- | The qualifier written at the use site, if any.
  Maybe Text ->
  -- | Operator being resolved.
  OpName ->
  -- | The imports that could hold the answer, each down to the module that
  -- actually stopped us.
  [ModuleChain]
unreadFor scope qualifier op =
  [ ModuleChain (importModule (unreadImport u) :| unreadBelow u)
  | u <- scopeUnread scope,
    reaches (unreadImport u),
    brings u,
    exports u
  ]
  where
    -- A module that says what it exports is taken at its word.
    exports u = maybe True (Set.member op) (unreadExports u)
    reaches i = case qualifier of
      Nothing -> not (importQualified i)
      Just q -> q == importAlias i
    -- What a @T(..)@ in the list stands for is often knowable even where
    -- the module's fixities are not: reading a module's declarations is
    -- one thing and settling every operator it passes on is another.
    brings u = case importNames (unreadImport u) of
      Nothing -> True
      Just (True, hidden) -> not (surelyNames (unreadCarries u) op hidden)
      Just (False, shown) -> mightBring (unreadCarries u) op shown

----------------------------------------------------------------------------
-- What could not be answered

-- | Why an operator's fixity could not be settled.
data Unknown
  = -- | These imports could not be read, each given down to the module that
    -- actually stopped us, and the declaration the answer depends on may be
    -- in any of them.
    NotRead (NonEmpty ModuleChain)
  | -- | Two modules in scope bring it in with different fixities, so which
    -- one applies cannot be read off the imports alone.
    Ambiguous
  deriving (Eq, Show)

-- | Every operator the module uses where its fixity decides the layout.
--
-- Only these positions. An operator chain in an expression and one in a type
-- are regrouped by precedence, so getting the precedence wrong changes what
-- the code means. Everywhere else—a section, the left-hand side of a
-- definition, an @infix@ declaration—the operator stands on its own and
-- nothing is regrouped around it.
operatorsUsed :: HsModule GhcPs -> [(Namespace, (Maybe Text, OpName))]
operatorsUsed hsModule =
  map (named InTerms) inExpressions <> map (named InTypes) inTypes
  where
    inExpressions =
      [ n
      | e :: HsExpr GhcPs <- listify (const True) hsModule,
        OpApp _ _ op _ <- [e],
        HsVar _ (L _ n) <- [unLoc op]
      ]
    inTypes =
      [ n
      | t :: HsType GhcPs <- listify (const True) hsModule,
        HsOpTy _ _ _ (L _ n) _ <- [t]
      ]
    named namespace n =
      (namespace, (qualifierOf n, OpName (T.pack (occNameString (rdrNameOcc n)))))

-- | The operators this module uses that the scope cannot settle, as the
-- module writes them.
--
-- Empty is the only acceptable answer: an operator whose fixity is not
-- known cannot be laid out, only guessed at.
unknownOperators :: Scope -> HsModule GhcPs -> [((Maybe Text, OpName), Unknown)]
unknownOperators scope hsModule =
  Map.toList (Map.fromList (mapMaybe unsettled (operatorsUsed hsModule)))
  where
    ambiguous namespace = Set.fromList (reachAmbiguous (reachIn namespace scope))
    unsettled (namespace, (qualifier, op)) =
      case settledFor scope namespace qualifier op of
        Just (answering, _)
          | Set.member (qualifier, op) (ambiguous answering) ->
              Just ((qualifier, op), Ambiguous)
          | otherwise -> Nothing
        Nothing -> case nonEmpty (unreadFor scope qualifier op) of
          Just missing -> Just ((qualifier, op), NotRead missing)
          Nothing -> Nothing

-- | An operator as a use site writes it, qualifier and all.
operatorSpelling :: Maybe Text -> OpName -> Text
operatorSpelling qualifier (OpName op) = maybe "" (<> ".") qualifier <> op

-- | Spell out where an unsettled operator may have come from, and the fact
-- that this run could not read any of it.
spellUnreadIn ::
  -- | Whether there is anybody there to see color.
  Palette ->
  -- | The chains, as 'Unresolved' gives them.
  NonEmpty ModuleChain ->
  Text
spellUnreadIn palette missing =
  T.intercalate " or " (map (spellModuleChain palette) (toList missing))
    <> ", "
    <> ofThose
  where
    ofThose = case toList missing of
      [_] -> "which this run could not read"
      [_, _] -> "neither of which this run could read"
      _ -> "none of which this run could read"

----------------------------------------------------------------------------
-- What reading a module established

-- | What reading a module established about its operators.
--
-- Declaring nothing is something a module did; being unreadable is
-- something that happened to us. Everything here turns on keeping those
-- apart, which is why this is two constructors rather than a map that
-- might be empty.
data Established
  = -- | It was read, and declares these.
    Declares Fixities
  | -- | It could not be read. The expensive answer of the two, because
    -- reaching it means exhausting every way of reading the module.
    --
    -- The name is the module below this one that stopped us, where the
    -- failure was not this module's own. One hop only: the module named
    -- carries its own, and following them is how a whole chain is got back.
    -- It is kept because it has to outlive the run that found it — a
    -- verdict of unreadable is cached, and a reason that were not cached
    -- with it would leave the second run with a worse account than the
    -- first.
    Unreadable (Maybe Text)
  deriving (Eq, Show)

-- | What reading a module established about its export list.
--
-- 'exportedOperators' answers the same question as @'Maybe' ('Set'
-- 'OpName')@, which is the shape 'resolveScope' wants. This is that answer
-- given a name, so that having one and never having asked can be told apart
-- where both have to be written down.
data Exported
  = -- | The list names these, and they are all the module can supply.
    Exports (Set OpName)
  | -- | Nothing that can be enumerated: the list hands whole modules on, or
    -- the source would not parse.
    Untellable
  deriving (Eq, Show)

-- | What 'resolveScope' makes of it.
exportedNames :: Exported -> Maybe (Set OpName)
exportedNames = \case
  Exports names -> Just names
  Untellable -> Nothing

-- | What to write down for an answer 'exportedOperators' gave.
asExported :: Maybe (Set OpName) -> Exported
asExported = maybe Untellable Exports
