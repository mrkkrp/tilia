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

    -- * Module declarations
    declaredFixities,
    declaredNames,
    moduleName,

    -- * Module exports
    ExportItem (..),
    moduleExports,
    declaredChildren,
    moduleChildren,

    -- * Module imports
    Import (..),
    ImportItem (..),
    moduleImports,
    mightBring,
    surelyNames,
    KnownModules (..),
    noKnownModules,
    Namespace (..),
    Fixities,
    inBothNamespaces,
    UnreadModule (..),
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
-- Module declarations

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
      TyClD _ ClassDecl{tcdSigs} -> concatMap (fromSig . unLoc) tcdSigs
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
        FamDecl _ FamilyDecl{fdLName} -> [opName (unLoc fdLName)]
        SynDecl{tcdLName} -> [opName (unLoc tcdLName)]
        DataDecl{tcdLName} -> [opName (unLoc tcdLName)]
        ClassDecl{tcdLName, tcdATs} ->
          opName (unLoc tcdLName)
            : [opName (unLoc (fdLName (unLoc f))) | f <- tcdATs]
      _ -> []
    terms = \case
      ValD _ b -> boundNames b
      SigD _ sig -> signedNames sig
      ForD _ f -> [opName (unLoc (fd_name f))]
      TyClD _ d@DataDecl{} -> membersOf d
      TyClD _ ClassDecl{tcdSigs} -> concatMap (classMethods . unLoc) tcdSigs
      _ -> []

-- | The methods a class signature declares.
classMethods :: Sig GhcPs -> [OpName]
classMethods = \case
  TypeSig _ ns _ -> fmap (opName . unLoc) ns
  ClassOpSig _ _ ns _ -> fmap (opName . unLoc) ns
  _ -> []

-- | The names a declaration carries under the name it declares: a data
-- type's constructors and record fields, a class's methods and the
-- families it keeps.
--
-- These are what @T(..)@ stands for, and each of them can carry a fixity of
-- its own—@:|@ is a constructor and @infixr 5@ all the same.
membersOf :: TyClDecl GhcPs -> [OpName]
membersOf = \case
  DataDecl{tcdDataDefn} -> concatMap (fromCon . unLoc) (consOf (dd_cons tcdDataDefn))
  ClassDecl{tcdSigs, tcdATs} ->
    concatMap (classMethods . unLoc) tcdSigs
      <> [opName (unLoc (fdLName (unLoc f))) | f <- tcdATs]
  _ -> []
  where
    consOf :: DataDefnCons (LConDecl GhcPs) -> [LConDecl GhcPs]
    consOf = toList

    fromCon :: ConDecl GhcPs -> [OpName]
    fromCon = \case
      ConDeclGADT{con_names} -> fmap (opName . unLoc) (toList con_names)
      ConDeclH98{con_name, con_args} ->
        opName (unLoc con_name) : fieldNames con_args

    fieldNames :: HsConDeclH98Details GhcPs -> [OpName]
    fieldNames = \case
      RecCon fields ->
        [ opName (unLoc (foLabel (unLoc n)))
        | f <- unLoc fields,
          n <- cdrf_names (unLoc f)
        ]
      _ -> []

-- | The names a signature is about, leaving fixity declarations aside.
signedNames :: Sig GhcPs -> [OpName]
signedNames = \case
  TypeSig _ ns _ -> fmap (opName . unLoc) ns
  ClassOpSig _ _ ns _ -> fmap (opName . unLoc) ns
  PatSynSig _ ns _ -> fmap (opName . unLoc) ns
  _ -> []

-- | Take a fixity as GHC presents it.
fromGhcFixity :: GHC.Fixity -> Fixity
fromGhcFixity (GHC.Fixity prec dir) = Fixity (fromGhcDirection dir) prec

-- | Take an associativity as GHC presents it.
fromGhcDirection :: GHC.FixityDirection -> Direction
fromGhcDirection = \case
  GHC.InfixL -> LeftAssoc
  GHC.InfixR -> RightAssoc
  GHC.InfixN -> NoAssoc

-- | Render a parsed name as an operator name.
opName :: RdrName -> OpName
opName = OpName . T.pack . occNameString . rdrNameOcc

-- | Every name a module defines itself.
--
-- Not the same question as 'declaredFixities', which is about @infix@
-- declarations. This one is asked of an export list: a name a module
-- exports and also defines needs no chasing, and one it merely reexports
-- does.
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
      FixSig _ (FixitySig _ ns _) -> fmap (opName . unLoc) ns
      sig -> signedNames sig

    fromTyCl = \case
      FamDecl _ (FamilyDecl{fdLName}) -> [opName (unLoc fdLName)]
      SynDecl{tcdLName} -> [opName (unLoc tcdLName)]
      d@DataDecl{tcdLName} -> opName (unLoc tcdLName) : membersOf d
      d@ClassDecl{tcdLName} -> opName (unLoc tcdLName) : membersOf d

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
      VarPat{} -> True
      _ -> False

-- | The module's own name, if it declares one.
moduleName :: HsModule GhcPs -> Maybe Text
moduleName = fmap (T.pack . moduleNameString . unLoc) . hsmodName

----------------------------------------------------------------------------
-- Module exports

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

-- | A module's export list, or 'Nothing' if it has none.
moduleExports :: HsModule GhcPs -> Maybe [ExportItem]
moduleExports =
  fmap (concatMap (fromIE . unLoc) . unLoc) . hsmodExports
  where
    fromIE = \case
      IEVar _ n _ -> [named n]
      IEThingAbs _ n _ -> [named n]
      IEThingAll _ n _ -> [as ExportAll n]
      IEThingWith _ n _ ns _ -> named n : fmap named ns
      IEModuleContents _ m -> [ExportModule (T.pack (moduleNameString (unLoc m)))]
      _ -> []
    named = as ExportName
    as item n =
      let rdr = ieWrappedName (unLoc n)
       in item (qualifierOf rdr) (opName rdr)

-- | The qualifier a name was written under.
qualifierOf :: RdrName -> Maybe Text
qualifierOf = \case
  Qual m _ -> Just (T.pack (moduleNameString m))
  _ -> Nothing

-- | What each type or class a module declares carries with it.
declaredChildren :: HsModule GhcPs -> Map OpName (Set OpName)
declaredChildren =
  Map.fromListWith Set.union . concatMap (fromDecl . unLoc) . hsmodDecls
  where
    fromDecl = \case
      TyClD _ d@DataDecl{tcdLName} -> [entry tcdLName d]
      TyClD _ d@ClassDecl{tcdLName} -> [entry tcdLName d]
      _ -> []
    entry name d = (opName (unLoc name), Set.fromList (membersOf d))

-- | What a module offers under each name, as its export list offers it.
moduleChildren :: HsModule GhcPs -> Map OpName (Set OpName)
moduleChildren hsModule = case hsmodExports hsModule of
  Nothing -> declared
  Just items -> Map.fromListWith Set.union (concatMap (fromIE . unLoc) (unLoc items))
  where
    declared = declaredChildren hsModule
    fromIE = \case
      IEThingAll _ n _ ->
        [(nameOf n, Map.findWithDefault Set.empty (nameOf n) declared)]
      IEThingWith _ n _ ns _ -> [(nameOf n, Set.fromList (fmap nameOf ns))]
      _ -> []
    nameOf = opName . ieWrappedName . unLoc

----------------------------------------------------------------------------
-- Module imports

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

-- | The imports of a module.
moduleImports ::
  -- | Whether @ImplicitPrelude@ is on.
  Choice "implicitPrelude" ->
  -- | Parsed module.
  HsModule GhcPs ->
  [Import]
moduleImports implicitPrelude hsModule = prelude <> written
  where
    written = fmap (fromDecl . unLoc) (hsmodImports hsModule)
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
  IEThingWith _ n _ ns _ -> Just (ImportedSome (nameOf n) (fmap nameOf ns))
  _ -> Nothing
  where
    nameOf :: LIEWrappedName GhcPs -> OpName
    nameOf = opName . ieWrappedName . unLoc

-- | Could this list bring the operator in?
mightBring ::
  -- | What each name in the list keeps under it, where that is known.
  Map OpName (Set OpName) ->
  -- | The operator being looked for.
  OpName ->
  -- | The entries of the import list.
  [ImportItem] ->
  Bool
mightBring carries op = any $ \case
  ImportedName n -> n == op
  ImportedSome parent ns -> parent == op || op `elem` ns
  ImportedAll parent -> maybe True (names parent) (Map.lookup parent carries)
  where
    names parent kids = parent == op || Set.member op kids

-- | Does this list certainly name the operator?
surelyNames ::
  -- | What each name in the list keeps under it, where that is known.
  Map OpName (Set OpName) ->
  -- | The operator being looked for.
  OpName ->
  -- | The entries of the import list.
  [ImportItem] ->
  Bool
surelyNames carries op = any $ \case
  ImportedName n -> n == op
  ImportedSome parent ns -> parent == op || op `elem` ns
  ImportedAll parent ->
    maybe
      (parent == op)
      (names parent)
      (Map.lookup parent carries)
  where
    names parent kids = parent == op || Set.member op kids

-- | What is known about the imported modules.
data KnownModules = KnownModules
  { -- | The fixities a module exports, or 'Nothing' if that could not be
    -- determined.
    knownFixities :: Text -> Maybe Fixities,
    -- | What a module keeps under each of its names, so that a @T(..)@ in
    -- an import list can be told what it brings in.
    knownChildren :: Text -> Map OpName (Set OpName),
    -- | The operators a module's export list names, following what it
    -- reexports. 'Nothing' where a module it hands on could not be read.
    knownExportNames :: Text -> Maybe (Set OpName),
    -- | The modules reading a module went through before giving up, the one
    -- it gave up on last. Asked only about modules 'knownFixities' could
    -- not answer for, and only so that a message can name the module that
    -- is really in the way.
    knownChain :: Text -> [Text]
  }

-- | No known modules.
noKnownModules :: KnownModules
noKnownModules =
  KnownModules
    { knownFixities = const Nothing,
      knownChildren = const Map.empty,
      knownExportNames = const Nothing,
      knownChain = const []
    }

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

-- | An import whose module could not be read, and what is known about it
-- regardless.
data UnreadModule = UnreadModule
  { -- | The import as written.
    unreadImport :: Import,
    -- | The operators its export list names, as 'knownExportNames' gives
    -- them.
    unreadExportNames :: Maybe (Set OpName),
    -- | What it keeps under each of its names, as 'knownChildren' gives
    -- them, for expanding a @T(..)@ in the import list.
    unreadChildren :: Map OpName (Set OpName),
    -- | The modules reading went through before giving up, as 'knownChain'
    -- gives them.
    unreadChain :: [Text]
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
  T.intercalate " → " (fmap (paint palette Place) (toList modules))

-- | Every fixity a module can see, and how.
data Scope = Scope
  { -- | What is in scope for an operator written among types.
    scopeInTypes :: Reach,
    -- | What is in scope for one written among terms.
    scopeInTerms :: Reach,
    -- | The imports whose modules could not be read, and what is
    -- nonetheless known about each.
    scopeUnread :: [UnreadModule]
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
    -- would have to be written to run into it: without a qualifier, or
    -- under the alias the disagreeing imports share.
    reachAmbiguous :: [(Maybe Text, OpName)]
  }
  deriving (Eq, Show)

-- | The half of a scope an operator written in this namespace is settled
-- against.
reachIn :: Namespace -> Scope -> Reach
reachIn = \case
  InTypes -> scopeInTypes
  InTerms -> scopeInTerms

-- | Work out what a module can see.
resolveScope ::
  -- | Whether @ImplicitPrelude@ is on.
  Choice "implicitPrelude" ->
  -- | What is known about the modules this one imports.
  KnownModules ->
  -- | Parsed module.
  HsModule GhcPs ->
  Scope
resolveScope implicitPrelude known hsModule =
  Scope
    { scopeInTypes = reachAmong InTypes,
      scopeInTerms = reachAmong InTerms,
      scopeUnread = unread
    }
  where
    KnownModules{knownFixities = exportsOf, knownChildren, knownExportNames, knownChain} = known
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
      [ UnreadModule
          { unreadImport = i,
            unreadExportNames = exportNamesOf (importModule i),
            unreadChildren = knownChildren (importModule i),
            unreadChain = knownChain (importModule i)
          }
      | i <- imports,
        Nothing <- [exportsOf (importModule i)]
      ]

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

-- | The fixities in one namespace, by the operator alone.
fixitiesIn :: Namespace -> Fixities -> Map OpName Fixity
fixitiesIn namespace declared =
  Map.fromList
    [ (op, fixity)
    | ((n, op), fixity) <- Map.toList declared,
      n == namespace
    ]

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
  | -- | Not established.
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
  case fixityInScope scope namespace qualifier op of
    Just (_, (fixity, provenance)) -> Resolved fixity provenance
    Nothing -> case nonEmpty (unreadThatMightDeclare scope qualifier op) of
      Nothing -> Resolved defaultFixity ReportDefault
      Just missing -> Unresolved missing

-- | What the scope itself has for a use, and the namespace it came from.
fixityInScope ::
  -- | The scope.
  Scope ->
  -- | The namespace the operator is written in.
  Namespace ->
  -- | The qualifier written at the use site, if any.
  Maybe Text ->
  -- | Operator to resolve.
  OpName ->
  -- | The fixity and where it came from, under the namespace that supplied
  -- it. 'Nothing' where the scope has no answer.
  Maybe (Namespace, (Fixity, Provenance))
fixityInScope scope namespace qualifier op =
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

-- | The unread imports that could have declared this operator.
unreadThatMightDeclare ::
  -- | The scope.
  Scope ->
  -- | The qualifier written at the use site, if any.
  Maybe Text ->
  -- | Operator being resolved.
  OpName ->
  -- | The imports that could hold the answer, each down to the module that
  -- actually stopped us.
  [ModuleChain]
unreadThatMightDeclare scope qualifier op =
  [ ModuleChain (importModule (unreadImport u) :| unreadChain u)
  | u <- scopeUnread scope,
    reaches (unreadImport u),
    brings u,
    exports u
  ]
  where
    exports u = maybe True (Set.member op) (unreadExportNames u)
    reaches i = case qualifier of
      Nothing -> not (importQualified i)
      Just q -> q == importAlias i
    brings u = case importNames (unreadImport u) of
      Nothing -> True
      Just (True, hidden) -> not (surelyNames (unreadChildren u) op hidden)
      Just (False, shown) -> mightBring (unreadChildren u) op shown

----------------------------------------------------------------------------
-- What could not be answered

-- | Why an operator's fixity could not be determined.
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
  fmap (named InTerms) inExpressions <> fmap (named InTypes) inTypes
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
      case fixityInScope scope namespace qualifier op of
        Just (answering, _)
          | Set.member (qualifier, op) (ambiguous answering) ->
              Just ((qualifier, op), Ambiguous)
          | otherwise -> Nothing
        Nothing -> case nonEmpty (unreadThatMightDeclare scope qualifier op) of
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
  T.intercalate " or " (fmap (spellModuleChain palette) (toList missing))
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
data Established
  = -- | It was read, and declares these.
    Declares Fixities
  | -- | It could not be read. The expensive answer of the two, because
    -- reaching it means exhausting every way of reading the module.
    Unreadable (Maybe Text)
  deriving (Eq, Show)

-- | What reading a module established about its export list.
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

-- | What to write down for an answer the reader worked out.
asExported :: Maybe (Set OpName) -> Exported
asExported = maybe Untellable Exports
