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
    spellFixity,

    -- * Module declarations
    declaredFixities,
    declaredNames,
    moduleName,

    -- * Module exports
    ExportItem (..),
    moduleExports,
    declaredMembers,
    listedMembers,

    -- * Module imports
    Import (..),
    ImportItem (..),
    moduleImports,
    maySupply,
    certainlyBrings,
    decides,
    Namespace (..),
    Fixities,
    inBothNamespaces,
    Certain (..),
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
    capturedUses,
    operatorSpelling,
    spellUnreadIn,
    spellDisagreement,

    -- * Module summaries
    ModuleSummary (..),
    summarize,

    -- * What reading a module established
    Established (..),
    unreadable,
    settlesEverything,
    unsettledThrough,
  )
where

import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import Data.Char (isUpper)
import Data.Choice (Choice, isTrue)
import Data.Foldable (toList)
import Data.Generics.Schemes (listify)
import Data.List (nub, sortOn, tails)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Hs hiding (Fixity, OpName)
import GHC.Types.Fixity qualified as GHC
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import GHC.Types.SrcLoc (GenLocated (..), unLoc)
import Tilia.Gathered (Gathered (..))
import Tilia.Palette (Color (Place), Palette, paint)
import Tilia.Span (Span, covers)
import Tilia.Span.Ghc (spanOf)
import Tilia.Utils (collected, spellList)

----------------------------------------------------------------------------
-- Fixities

-- | An operator, spelled as it appears in an @infix@ declaration: @<+>@, or
-- @div@ for a function used infix in backticks.
newtype OpName = OpName Text
  deriving (Eq, Ord, Show, Generic)

instance NFData OpName

-- | Which way an operator associates.
data Direction = LeftAssoc | RightAssoc | NoAssoc
  deriving (Eq, Show, Generic)

instance NFData Direction

-- | A fixity: how tightly an operator binds, and which way it associates.
data Fixity = Fixity
  { fixityDirection :: Direction,
    fixityPrecedence :: Int
  }
  deriving (Eq, Show, Generic)

instance NFData Fixity

-- | What an operator with no declaration in scope means: @infixl 9@.
defaultFixity :: Fixity
defaultFixity = Fixity LeftAssoc 9

-- | A fixity, written the way it would be declared.
spellFixity :: Fixity -> Text
spellFixity (Fixity direction precedence) =
  which direction <> " " <> T.pack (show precedence)
  where
    which = \case
      LeftAssoc -> "infixl"
      RightAssoc -> "infixr"
      NoAssoc -> "infix"

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
      TyClD _ d@DataDecl{} -> fmap snd (membersOf d)
      TyClD _ ClassDecl{tcdSigs} -> concatMap (classMethods . unLoc) tcdSigs
      _ -> []

-- | The methods a class signature declares.
classMethods :: Sig GhcPs -> [OpName]
classMethods = \case
  TypeSig _ ns _ -> fmap (opName . unLoc) ns
  ClassOpSig _ _ ns _ -> fmap (opName . unLoc) ns
  _ -> []

-- | The members of the type or class a declaration declares, by
-- namespace: a data type's constructors and record fields, a class's
-- methods and associated families.
--
-- These are what @T(..)@ stands for, and each of them can carry a fixity of
-- its own—@:|@ is a constructor and @infixr 5@ all the same.
membersOf :: TyClDecl GhcPs -> [(Namespace, OpName)]
membersOf = \case
  DataDecl{tcdDataDefn} ->
    fmap (InTerms,) (concatMap (fromCon . unLoc) (consOf (dd_cons tcdDataDefn)))
  ClassDecl{tcdSigs, tcdATs} ->
    fmap (InTerms,) (concatMap (classMethods . unLoc) tcdSigs)
      <> [(InTypes, opName (unLoc (fdLName (unLoc f)))) | f <- tcdATs]
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
declaredNames :: HsModule GhcPs -> Set (Namespace, OpName)
declaredNames hsModule = Set.map (InTypes,) types <> Set.map (InTerms,) terms
  where
    (types, terms) = declaredNamespaces hsModule

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
  = -- | A name, which may or may not be declared in this module, in the
    -- namespace the list writes it in, under the qualifier it was written
    -- with if it was written with one.
    ExportName Namespace (Maybe Text) OpName
  | -- | @T(..)@: the name, and with it whatever the module has to give
    -- under that name. Which names those are cannot be read off the list;
    -- it takes the declaration of @T@, or the module @T@ came from.
    ExportAll (Maybe Text) OpName
  | -- | @T(a, b)@: the name, and the members written out beside it.
    ExportSome (Maybe Text) OpName [OpName]
  | -- | @module M@, re-exporting everything that module brought in.
    ExportModule Text
  deriving (Eq, Show, Generic)

instance NFData ExportItem

-- | A module's export list, or 'Nothing' if it has none.
moduleExports :: HsModule GhcPs -> Maybe [ExportItem]
moduleExports =
  fmap (concatMap (fromIE . unLoc) . unLoc) . hsmodExports
  where
    fromIE = \case
      IEVar _ n _ -> [as (ExportName InTerms) n]
      IEThingAbs _ n _ -> [as (ExportName InTypes) n]
      IEThingAll _ n _ -> [as ExportAll n]
      IEThingWith _ n _ ns _ ->
        [as ExportSome n (fmap (opName . ieWrappedName . unLoc) ns)]
      IEModuleContents _ m -> [ExportModule (T.pack (moduleNameString (unLoc m)))]
      _ -> []
    as item n =
      let rdr = ieWrappedName (unLoc n)
       in item (qualifierOf rdr) (opName rdr)

-- | The qualifier a name was written under.
qualifierOf :: RdrName -> Maybe Text
qualifierOf = \case
  Qual m _ -> Just (T.pack (moduleNameString m))
  _ -> Nothing

-- | The members of each type or class a module declares.
declaredMembers :: HsModule GhcPs -> Map OpName (Set (Namespace, OpName))
declaredMembers =
  Map.fromListWith Set.union . concatMap (fromDecl . unLoc) . hsmodDecls
  where
    fromDecl = \case
      TyClD _ d@DataDecl{tcdLName} -> [entry tcdLName d]
      TyClD _ d@ClassDecl{tcdLName} -> [entry tcdLName d]
      _ -> []
    entry name d = (opName (unLoc name), Set.fromList (membersOf d))

-- | The members a module's export list offers with each name.
listedMembers :: HsModule GhcPs -> Map OpName (Set OpName)
listedMembers hsModule = case hsmodExports hsModule of
  Nothing -> declared
  Just items -> Map.fromListWith Set.union (concatMap (fromIE . unLoc) (unLoc items))
  where
    declared = Map.map (Set.map snd) (declaredMembers hsModule)
    fromIE = \case
      IEThingAll _ n _ -> [(nameOf n, kids) | Just kids <- [Map.lookup (nameOf n) declared]]
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
  deriving (Eq, Show, Generic)

instance NFData Import

-- | One entry of an import list.
data ImportItem
  = -- | A plain name.
    ImportedName OpName
  | -- | @T(..)@: the name, and every member the module offers with it.
    ImportedAll OpName
  | -- | @T(a, b)@: the name and the members written out beside it.
    ImportedSome OpName [OpName]
  deriving (Eq, Show, Generic)

instance NFData ImportItem

-- | The imports of a module, all its configurations taken together.
moduleImports ::
  -- | Whether @ImplicitPrelude@ is on.
  Choice "implicitPrelude" ->
  -- | The module's configurations, parsed.
  NonEmpty (HsModule GhcPs) ->
  [Import]
moduleImports implicitPrelude configurations = prelude <> written
  where
    written =
      nub (fmap (fromDecl . unLoc) (sortOn spanOf (concatMap hsmodImports configurations)))
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

-- | Could this import bring the operator in, as far as its list says?
--
-- A @T(..)@ whose members are not known is taken to bring anything in, and
-- to hide nothing but @T@ itself.
mayBring ::
  -- | The members of each name in the list, where they are known.
  Map OpName (Set OpName) ->
  -- | The operator being looked for.
  OpName ->
  Import ->
  Bool
mayBring members op i = case importNames i of
  Nothing -> True
  Just (True, hidden) -> not (any (namedBy members False op) hidden)
  Just (False, shown) -> any (namedBy members True op) shown

-- | Does an item of an import list name the operator, taking a @T(..)@ whose
-- members are not known to name it or not as told?
namedBy ::
  -- | The members of each name in the list, where they are known.
  Map OpName (Set OpName) ->
  -- | What a @T(..)@ whose members are not known is taken to say.
  Bool ->
  -- | The operator being looked for.
  OpName ->
  ImportItem ->
  Bool
namedBy members unknown op = \case
  ImportedName n -> n == op
  ImportedSome parent ns -> parent == op || op `elem` ns
  ImportedAll parent ->
    parent == op || maybe unknown (Set.member op) (Map.lookup parent members)

-- | Could this import supply the operator to a use written under this
-- qualifier?
maySupply ::
  -- | The members of each name in the import's list, where they are
  -- known.
  Map OpName (Set OpName) ->
  -- | The qualifier written at the use site, if any.
  Maybe Text ->
  -- | The operator being looked for.
  OpName ->
  Import ->
  Bool
maySupply members qualifier op i =
  maybe (not (importQualified i)) (== importAlias i) qualifier
    && mayBring members op i

-- | Does this import bring the name in for certain, as far as its list says?
--
-- Where the list could leave the name out, it is taken to: a @T(..)@ whose
-- members are not known hides anything, and of what a list shows, only a
-- variable written on its own, a member written under its type, and a member
-- of a @T(..)@ whose members are known count.
certainlyBrings ::
  -- | The members of each name in the import's list, where they are
  -- known.
  Map OpName (Set OpName) ->
  -- | The name, and the namespace it is in.
  (Namespace, OpName) ->
  Import ->
  Bool
certainlyBrings members (namespace, op) i = case importNames i of
  Nothing -> True
  Just (True, hidden) -> not (any (namedBy members True op) hidden)
  Just (False, shown) -> namespace == InTerms && any listed shown
  where
    listed = \case
      ImportedName n -> n == op && isVariable op
      ImportedSome _ ns -> op `elem` ns
      ImportedAll parent -> maybe False (Set.member op) (Map.lookup parent members)
    isVariable (OpName t) = case T.uncons t of
      Just (c, _) -> not (isUpper c) && c /= ':'
      Nothing -> False

-- | Does this import decide the name's fixity, by certainly bringing it
-- in from a module that settles it?
--
-- If so, no other import can give a use of the name another fixity: it
-- either brings in the same thing or makes the use ambiguous.
decides ::
  -- | What reading the module imported established.
  Established ->
  -- | The name, and the namespace it is in.
  (Namespace, OpName) ->
  Import ->
  Bool
decides established name i =
  Set.member name (certainNames (establishedCertain established))
    && certainlyBrings (establishedMembers established) name i
    && null (unsettledThrough established name)

-- | Which of Haskell's two namespaces an operator is written in.
data Namespace = InTypes | InTerms
  deriving (Eq, Ord, Show, Generic)

instance NFData Namespace

-- | The fixities a module offers.
type Fixities = Map (Namespace, OpName) Fixity

-- | Take fixities that say nothing about namespaces to govern both.
inBothNamespaces :: Map OpName Fixity -> Fixities
inBothNamespaces declared =
  Map.fromList
    [ ((namespace, op), fixity)
    | (op, fixity) <- Map.toList declared,
      namespace <- [InTypes, InTerms]
    ]

-- | What a module certainly brings into scope for a module that imports
-- it whole.
data Certain = Certain
  { -- | Every name it certainly exports.
    certainNames :: Set (Namespace, OpName),
    -- | The members it certainly exports with each type or class.
    certainMembers :: Map OpName (Set (Namespace, OpName))
  }
  deriving (Eq, Show, Generic)

instance NFData Certain

instance Semigroup Certain where
  a <> b =
    Certain
      { certainNames = certainNames a <> certainNames b,
        certainMembers =
          Map.unionWith Set.union (certainMembers a) (certainMembers b)
      }

instance Monoid Certain where
  mempty = Certain Set.empty Map.empty

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
    -- | The imports whose modules leave the fixities of some names
    -- unsettled, each with what reading its module established.
    scopeUnsettled :: [(Import, Established)]
  }
  deriving (Eq, Show)

-- | What one namespace of a scope holds.
data Reach = Reach
  { -- | Reachable without qualification, with where it came from.
    reachUnqualified :: Map OpName (Fixity, Provenance),
    -- | Reachable as @M.op@, keyed by the alias actually written—or by the
    -- module's own name, under which its own declarations are reachable.
    reachQualified :: Map (Text, OpName) (Fixity, Provenance),
    -- | Operators the imports bring in with different fixities, as they
    -- would have to be written to run into it—without a qualifier, or under
    -- the alias the disagreeing imports share—with the module each import
    -- names and the fixity it brings.
    reachAmbiguous :: Map (Maybe Text, OpName) (NonEmpty (Text, Fixity)),
    -- | The uses a module that was read decides in this namespace, as they
    -- would be written: of a name the module defines itself, or one an import
    -- that was read certainly brings in, which no unread import can give
    -- another fixity.
    reachDecided :: Set (Maybe Text, OpName)
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
  -- | What reading each module this one imports established.
  (Text -> Established) ->
  -- | The module's configurations, parsed.
  NonEmpty (HsModule GhcPs) ->
  Scope
resolveScope implicitPrelude known configurations =
  Scope
    { scopeInTypes = reachAmong InTypes,
      scopeInTerms = reachAmong InTerms,
      scopeUnsettled =
        [ (i, established)
        | i <- imports,
          let established = known (importModule i),
          not (settlesEverything established)
        ]
    }
  where
    imports = moduleImports implicitPrelude configurations
    declared = Map.unions (fmap declaredFixities configurations)

    reachAmong namespace =
      Reach
        { reachUnqualified = Map.union own (Map.map settled unqualified),
          reachQualified = qualified,
          reachAmbiguous =
            Map.union
              (Map.mapKeys (Nothing,) (Map.mapMaybe disagreeing unqualified))
              (Map.mapKeys (first Just) (Map.mapMaybe disagreeing qualifiedFrom)),
          reachDecided =
            Set.fromList $
              [ (qualifier, op)
              | c <- toList configurations,
                op <- Set.toList (inNamespace (declaredNamespaces c)),
                qualifier <- Nothing : fmap Just ownNames
              ]
                <> [ (qualifier, op)
                   | i <- imports,
                     let established = known (importModule i),
                     name@(n, op) <- Set.toList (certainNames (establishedCertain established)),
                     n == namespace,
                     decides established name i,
                     qualifier <- [Nothing | not (importQualified i)] <> [Just (importAlias i)]
                   ]
        }
      where
        inNamespace = case namespace of
          InTypes -> fst
          InTerms -> snd
        ownNames = concatMap (toList . moduleName) configurations
        own = Map.map (,DeclaredHere) (fixitiesIn namespace declared)
        offered m = fixitiesIn namespace (establishedFixities (known m))
        unqualified =
          Map.unionsWith
            (<>)
            [ visible offered i
            | i <- imports,
              not (importQualified i)
            ]
        qualified = Map.union ownQualified (Map.map settled qualifiedFrom)
        ownQualified =
          Map.fromList
            [ ((m, op), entry)
            | m <- ownNames,
              (op, entry) <- Map.toList own
            ]
        qualifiedFrom =
          Map.unionsWith
            (<>)
            [ Map.mapKeys (importAlias i,) (visible offered i)
            | i <- imports
            ]

    settled ((m, fixity) :| _) = (fixity, DeclaredIn m)

    disagreeing offers@((_, fixity) :| _)
      | all ((== fixity) . snd) offers = Nothing
      | otherwise = Just (NE.nub offers)

    visible offered i =
      Map.filterWithKey
        (\op _ -> mayBring (establishedMembers (known (importModule i))) op i)
        (Map.map (\fixity -> (importModule i, fixity) :| []) (offered (importModule i)))

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
  | -- | The language itself, as for @:@, whatever is in scope.
    BuiltIn
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
    Nothing -> case nonEmpty (unreadThatMightDeclare scope namespace qualifier op) of
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
fixityInScope scope namespace qualifier op
  | Just fixity <- Map.lookup op builtInSyntax =
      Just (namespace, (fixity, BuiltIn))
  | otherwise =
      case mapMaybe found (namespace : promotedFrom namespace) of
        (answer : _) -> Just answer
        [] -> Nothing
  where
    found n =
      (n,) <$> case qualifier of
        Nothing -> Map.lookup op (reachUnqualified (reachIn n scope))
        Just q -> Map.lookup (q, op) (reachQualified (reachIn n scope))

-- | The operators the language gives a fixity, rather than a declaration
-- anywhere: @:@ alone.
builtInSyntax :: Map OpName Fixity
builtInSyntax = Map.singleton (OpName ":") (Fixity RightAssoc 5)

-- | The namespaces a use written in this one may also refer to.
promotedFrom :: Namespace -> [Namespace]
promotedFrom = \case
  InTypes -> [InTerms]
  InTerms -> []

-- | The unread imports that could have declared this operator.
unreadThatMightDeclare ::
  -- | The scope.
  Scope ->
  -- | The namespace the operator is written in.
  Namespace ->
  -- | The qualifier written at the use site, if any.
  Maybe Text ->
  -- | Operator being resolved.
  OpName ->
  -- | The imports that could hold the answer, each down to the module that
  -- actually stopped us, and each module once.
  [ModuleChain]
unreadThatMightDeclare scope namespace qualifier op
  | null blamed = []
  | Set.member (qualifier, op) (reachDecided (reachIn namespace scope)) = []
  | otherwise = nub blamed
  where
    blamed =
      [ ModuleChain (importModule i :| chain)
      | (i, established) <- scopeUnsettled scope,
        maySupply (establishedMembers established) qualifier op i,
        n <- namespace : promotedFrom namespace,
        chain <- unsettledThrough established (n, op)
      ]

----------------------------------------------------------------------------
-- What could not be answered

-- | Why an operator's fixity could not be determined.
data Unknown
  = -- | These imports could not be read, each given down to the module that
    -- actually stopped us, and the declaration the answer depends on may be
    -- in any of them.
    NotRead (NonEmpty ModuleChain)
  | -- | The imports in scope bring it in with different fixities, given as
    -- the module each import names and the fixity it brings, so which one
    -- applies cannot be read off the imports alone.
    Ambiguous (NonEmpty (Text, Fixity))
  deriving (Eq, Show)

-- | Every operator the module uses where its fixity decides the layout and
-- the scope has to settle it, which is all of them but the uses
-- 'capturedUses' settles.
--
-- Only these positions. An operator chain in an expression and one in a type
-- are regrouped by precedence, so getting the precedence wrong changes what
-- the code means. Everywhere else—a section, the left-hand side of a
-- definition, an @infix@ declaration—the operator stands on its own and
-- nothing is regrouped around it.
operatorsUsed :: Gathered -> [(Namespace, (Maybe Text, OpName))]
operatorsUsed found =
  fmap (named InTerms) inExpressions <> fmap (named InTypes) inTypes
  where
    captured = capturedUses found
    inExpressions =
      [ n
      | OpApp _ _ op _ <- gatheredExpressions found,
        HsVar _ (L _ n) <- [unLoc op],
        maybe True (`Map.notMember` captured) (spanOf op)
      ]
    inTypes =
      [n | HsOpTy _ _ _ (L _ n) _ <- gatheredTypes found]
    named namespace n =
      (namespace, (qualifierOf n, OpName (T.pack (occNameString (rdrNameOcc n)))))

-- | The uses of an operator in an expression that a local binding captures,
-- by where the operator is written, with the fixity the binding's group
-- declares for it, or @infixl 9@.
--
-- A use is captured where it falls within what a binder is in scope over,
-- which is read off spans: the right-hand sides and the @where@ of a match,
-- a @let@, the statements after a bind. Bindings that are in scope more
-- widely than that, as in @mdo@, are passed over, and so is a use that
-- bindings with different fixities could each capture.
capturedUses :: Gathered -> Map Span Fixity
capturedUses found =
  Map.fromList
    [ (s, fixity)
    | (s, name) <- uses,
      [fixity] <- [nub [f | (f, region) <- Map.findWithDefault [] name binders, covers region s]]
    ]
  where
    uses =
      [ (s, opName n)
      | OpApp _ _ op _ <- gatheredExpressions found,
        HsVar _ (L _ n@(Unqual _)) <- [unLoc op],
        Just s <- [spanOf op]
      ]
    binders =
      Map.fromListWith
        (<>)
        [ (name, [(f, region)])
        | (name, f, regions) <- concatMap fromMatch (gatheredMatches found) <> concatMap fromExpression (gatheredExpressions found),
          Set.member name used,
          Just region <- regions
        ]
    used = Set.fromList (fmap snd uses)

    fromMatch (Match _ _ (L _ pats) (GRHSs _ rhss binds)) =
      [ (name, f, fmap spanOf (toList rhss) <> bindingSpans binds)
      | (name, f) <- patternBinders pats <> localBinders binds
      ]
        <> concatMap guarded rhss
    fromExpression = \case
      HsLet _ binds body ->
        [(name, f, bindingSpans binds <> [spanOf body]) | (name, f) <- localBinders binds]
      HsDo _ _ (L _ stmts) -> inSequence stmts []
      HsMultiIf _ rhss -> concatMap guarded rhss
      _ -> []
    guarded (L _ (GRHS _ guards body)) = inSequence guards [spanOf body]

    inSequence stmts after =
      concat
        [ case unLoc stmt of
            BindStmt _ pat _ ->
              [(name, f, fmap spanOf later <> after) | (name, f) <- patternBinders [pat]]
            LetStmt _ binds ->
              [(name, f, fmap spanOf (stmt : later) <> after) | (name, f) <- localBinders binds]
            _ -> []
        | stmt : later <- tails stmts
        ]

    patternBinders pats =
      [(opName n, defaultFixity) | n <- collectPatsBinders CollNoDictBinders pats]
    localBinders binds =
      [ (opName n, Map.findWithDefault defaultFixity (opName n) declared)
      | let declared = localFixities binds,
        n <- collectLocalBinders CollNoDictBinders binds
      ]
    localFixities = \case
      HsValBinds _ (ValBinds _ _ sigs) ->
        Map.fromList
          [ (opName (unLoc n), fromGhcFixity fixity)
          | L _ (FixSig _ (FixitySig _ names fixity)) <- sigs,
            n <- names
          ]
      _ -> Map.empty
    bindingSpans = \case
      HsValBinds _ (ValBinds _ bs _) -> fmap spanOf bs
      _ -> []

-- | The operators this module uses that the scope cannot settle, as the
-- module writes them.
--
-- Empty is the only acceptable answer: an operator whose fixity is not
-- known cannot be laid out, only guessed at.
unknownOperators :: Scope -> NonEmpty Gathered -> [((Maybe Text, OpName), Unknown)]
unknownOperators scope found =
  Map.toList (Map.fromList (mapMaybe unsettled (concatMap operatorsUsed found)))
  where
    unsettled (namespace, (qualifier, op)) =
      case fixityInScope scope namespace qualifier op of
        Just (answering, _) ->
          ((qualifier, op),) . Ambiguous
            <$> Map.lookup (qualifier, op) (reachAmbiguous (reachIn answering scope))
        Nothing -> case nonEmpty (unreadThatMightDeclare scope namespace qualifier op) of
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

-- | Spell out the fixities an ambiguous operator is brought in with, and the
-- modules that bring each.
spellDisagreement ::
  -- | Whether there is anybody there to see color.
  Palette ->
  -- | What each import brings, as 'Ambiguous' gives it.
  NonEmpty (Text, Fixity) ->
  Text
spellDisagreement palette offers =
  case fmap bringing (collected [(fixity, m) | (m, fixity) <- toList offers]) of
    [one, other] -> one <> " but " <> other
    each -> spellList each
  where
    bringing (fixity, ms) =
      spellFixity fixity <> " in " <> spellList (fmap (paint palette Place) ms)

----------------------------------------------------------------------------
-- Module summaries

-- | Summary of a module: raw facts (per CPP configuration) about a module
-- we read from source. This is built for both local modules and dependency
-- modules when they come from tarballs. This is an optimization
-- mechanism—we create a summary once and then share it during a run. For
-- local modules it is also persisted in the cache on disk.
data ModuleSummary = ModuleSummary
  { -- | The name it gives itself, if it gives one.
    summaryName :: Maybe Text,
    -- | Its export list, if it has one.
    summaryExports :: Maybe [ExportItem],
    -- | Its imports, an implicit Prelude among them.
    summaryImports :: [Import],
    -- | The fixities it declares.
    summaryFixities :: Fixities,
    -- | Every name it defines.
    summaryNames :: Set (Namespace, OpName),
    -- | The members of each type or class it declares.
    summaryDeclaredMembers :: Map OpName (Set (Namespace, OpName)),
    -- | The members its export list offers with each name.
    summaryListedMembers :: Map OpName (Set OpName)
  }
  deriving (Eq, Show, Generic)

instance NFData ModuleSummary

-- | Summarize a module.
summarize ::
  -- | Whether @ImplicitPrelude@ is on.
  Choice "implicitPrelude" ->
  -- | Parsed module.
  HsModule GhcPs ->
  -- | Module summary.
  ModuleSummary
summarize implicitPrelude hsModule =
  ModuleSummary
    { summaryName = moduleName hsModule,
      summaryExports = moduleExports hsModule,
      summaryImports = moduleImports implicitPrelude (pure hsModule),
      summaryFixities = declaredFixities hsModule,
      summaryNames = declaredNames hsModule,
      summaryDeclaredMembers = declaredMembers hsModule,
      summaryListedMembers = listedMembers hsModule
    }

----------------------------------------------------------------------------
-- What reading a module established

-- | What reading a module established about the names it exports.
data Established = Established
  { -- | The fixities declared for the names it exports, leaving out the ones
    -- 'establishedUnsettled' leaves unsettled.
    establishedFixities :: Fixities,
    -- | The names whose fixities could not be established, under the way
    -- down to the module where reading gave up, which is empty where that
    -- is this module.
    establishedUnsettled :: Map [Text] (Set (Namespace, OpName)),
    -- | The ways down through which it may export names that cannot be
    -- told, which leaves every name it does not certainly bring in
    -- unsettled.
    establishedUntold :: Set [Text],
    -- | What it certainly brings in for a module that imports it whole.
    establishedCertain :: Certain,
    -- | The members of each of its names, so that a @T(..)@ in an import
    -- list can be told what it brings in. Every type 'establishedCertain'
    -- gives members of is among them.
    establishedMembers :: Map OpName (Set OpName)
  }
  deriving (Eq, Show, Generic)

instance NFData Established

instance Semigroup Established where
  a <> b =
    Established
      { establishedFixities = Map.union (establishedFixities a) (establishedFixities b),
        establishedUnsettled =
          Map.unionWith Set.union (establishedUnsettled a) (establishedUnsettled b),
        establishedUntold = Set.union (establishedUntold a) (establishedUntold b),
        establishedCertain = establishedCertain a <> establishedCertain b,
        establishedMembers =
          Map.unionWith Set.union (establishedMembers a) (establishedMembers b)
      }

instance Monoid Established where
  mempty = Established Map.empty Map.empty Set.empty mempty Map.empty

-- | What is established about a module that could not be read: nothing.
unreadable :: Established
unreadable = mempty{establishedUntold = Set.singleton []}

-- | Does reading a module settle the fixity of every name it exports?
settlesEverything :: Established -> Bool
settlesEverything established =
  Map.null (establishedUnsettled established) && Set.null (establishedUntold established)

-- | The ways down to a module that could not be read that leave the fixity
-- of a name unsettled.
unsettledThrough :: Established -> (Namespace, OpName) -> [[Text]]
unsettledThrough established name =
  [chain | (chain, names) <- Map.toList (establishedUnsettled established), Set.member name names]
    <> [ chain
       | Set.notMember name (certainNames (establishedCertain established)),
         chain <- Set.toList (establishedUntold established)
       ]
