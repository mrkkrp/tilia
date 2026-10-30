{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Reading a module's operators out of interface files.
module Tilia.Fixity.Interface
  ( Interface (..),
    readInterface,
    parseInterface,
    fromHiFile,
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString qualified as BS
import Data.Char (isUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Tilia.Fixity
import Tilia.Fixity.HiFile (HiExport (..), HiFile (..), HiName (..), decodeHiFile)
import Tilia.Process (readProgramOutput)
import Tilia.Utils (blocksOf, indented, quietly)

-- | What an interface says about the operators a module offers.
data Interface = Interface
  { -- | The fixities the module declares itself for the names it exports,
    -- by the namespace each governs.
    interfaceDeclares :: Fixities,
    -- | The names it reexports, each with the source module and the
    -- namespace it is exported in.
    interfaceReexports :: [(Text, Namespace, OpName)],
    -- | The members it exports with each name, which is what @T(..)@ in an
    -- import list stands for.
    interfaceMembers :: Map OpName (Set OpName),
    -- | What it certainly brings into scope, where the interface was decoded
    -- rather than read off what @ghc --show-iface@ prints.
    interfaceExports :: Maybe Certain
  }
  deriving (Eq, Show, Generic)

instance NFData Interface

-- | Read a module's interface file.
--
-- The file is decoded directly where it can be, and put to @ghc
-- --show-iface@ otherwise.
readInterface ::
  -- | The module the file is supposed to hold.
  Text ->
  -- | The file.
  FilePath ->
  IO (Maybe Interface)
readInterface modName path =
  quietly Nothing $
    decodeHiFile <$> BS.readFile path >>= \case
      Right hi -> pure (either (const Nothing) Just (fromHiFile modName hi))
      Left _ ->
        readProgramOutput "ghc" ["--show-iface", path] >>= \case
          Nothing -> pure Nothing
          Just out -> pure (parseInterface modName out)

-- | What a decoded interface file says, if it is this module's.
fromHiFile :: Text -> HiFile -> Either Text Interface
fromHiFile modName HiFile{..}
  | hiModule /= modName = Left ("the interface of " <> hiModule)
  | otherwise =
      Right
        Interface
          { interfaceDeclares =
              Map.fromList
                [ ((namespace, op), fixity)
                | (namespace, op, fixity) <- hiFixities,
                  Set.member (namespace, op) exportedHere
                ],
            interfaceReexports =
              [ (m, namespace, op)
              | HiName m namespace op <- concatMap exportedNames hiExports,
                m /= hiModule
              ],
            interfaceMembers = Map.map (Set.map snd) members,
            interfaceExports =
              Just
                Certain
                  { certainNames = exportedHere,
                    certainMembers = members
                  }
          }
  where
    exported = mapMaybe known (concatMap exportedNames hiExports)
    exportedHere =
      Set.fromList [(namespace, op) | (_, namespace, op) <- exported]
    members =
      Map.fromListWith
        Set.union
        [ (op, Set.fromList [(namespace, kid) | (_, namespace, kid) <- kids])
        | AvailTC p ns <- hiExports,
          Just parent@(_, _, op) <- [known p],
          let kids = filter (/= parent) (mapMaybe known ns)
        ]
    exportedNames = \case
      Avail n -> [n]
      AvailTC _ ns -> ns
    known = \case
      HiName m namespace op -> Just (Just m, namespace, op)
      BuiltInSyntax namespace op -> Just (Nothing, namespace, op)
      Unneeded -> Nothing

-- | Read what @ghc --show-iface@ printed, if it is this module's interface.
parseInterface :: Text -> Text -> Maybe Interface
parseInterface modName out
  | not (any holdsModule (T.lines out)) = Nothing
  | otherwise =
      Just
        Interface
          { interfaceDeclares =
              namespaced
                (typeNamesIn out)
                ( Map.restrictKeys
                    fixities
                    (foldMap ownExports (sectionsNamed "exports:"))
                ),
            interfaceReexports =
              [ (m, namespace, op)
              | (m, op) <- concatMap reexports (sectionsNamed "exports:"),
                namespace <- [InTypes, InTerms]
              ],
            interfaceMembers =
              Map.unionsWith Set.union (fmap membersIn (sectionsNamed "exports:")),
            interfaceExports = Nothing
          }
  where
    holdsModule l = case T.words l of
      ("interface" : m : _) -> m == modName
      _ -> False
    sectionsNamed name = [body | (heading, body) <- sections out, heading == name]
    fixities =
      Map.fromListWith
        (flip (<>))
        [ (op, [fixity])
        | (op, fixity) <- concatMap declared (sectionsNamed "fixities")
        ]
    declared = mapMaybe fixityEntry . T.splitOn ","
    ownExports section =
      Set.fromList
        [OpName name | name <- exportedIn section, isNothing (moduleOf name)]
    reexports section =
      [ (m, OpName name)
      | Just (m, name) <- fmap moduleOf (exportedIn section),
        not (T.null name)
      ]
    exportedIn section =
      [ name
      | (parent, parentExported, kids) <- exportEntries section,
        name <- [parent | parentExported] <> kids
      ]
    membersIn section =
      Map.fromListWith
        Set.union
        [ (nameOnly parent, Set.fromList (fmap nameOnly kids))
        | (parent, _, kids@(_ : _)) <- exportEntries section
        ]

-- | The names an interface declares as types.
typeNamesIn :: Text -> Set OpName
typeNamesIn out =
  Set.fromList
    [ nameOnly (T.dropWhileEnd (== ')') (T.dropWhile (== '(') name))
    | l <- T.lines out,
      indented l,
      (keyword : rest) <- [T.words l],
      keyword `elem` (["data", "type", "newtype", "class"] :: [Text]),
      name <- take 1 (dropWhile (`elem` (["family", "role", "instance"] :: [Text])) rest)
    ]

-- | Sort declared fixities into the namespaces they govern: of two for one
-- name, GHC lists the term's first.
namespaced :: Set OpName -> Map OpName [Fixity] -> Fixities
namespaced types declared =
  Map.fromList
    [ ((namespace, op), fixity)
    | (op, fixities) <- Map.toList declared,
      (namespace, fixity) <- zip (governed op fixities) fixities
    ]
  where
    governed op = \case
      [_, _] -> [InTerms, InTypes]
      _ -> [if Set.member op types then InTypes else InTerms]

-- | Split the output into sections.
sections :: Text -> [(Text, Text)]
sections out =
  [ (heading, T.unwords (opening : body))
  | (l, body) <- blocksOf (T.lines out),
    let (heading, opening) = T.breakOn " " l
  ]

-- | One entry of a @fixities@ line: @infixl 9 !@ and the like.
fixityEntry :: Text -> Maybe (OpName, Fixity)
fixityEntry entry = case T.words entry of
  [direction, precedence, op] -> do
    d <- case direction of
      "infixl" -> Just LeftAssoc
      "infixr" -> Just RightAssoc
      "infix" -> Just NoAssoc
      _ -> Nothing
    p <- readPrecedence precedence
    pure (OpName op, Fixity d p)
  _ -> Nothing

-- | Split an exports section into its entries, keeping the members an entry
-- wears in braces with the name they belong to, and whether that name is
-- exported itself, which a bar before the braces, as in @T|{...}@, says it
-- is not.
exportEntries :: Text -> [(Text, Bool, [Text])]
exportEntries = go
  where
    go text = case T.uncons (T.dropWhile (== ' ') text) of
      Nothing -> []
      Just _ ->
        let trimmed = T.dropWhile (== ' ') text
            (name, rest) = T.break (\c -> c == ' ' || c == '{') trimmed
         in case T.uncons rest of
              Just ('{', inside) ->
                let (kids, after) = T.break (== '}') inside
                    entry = case T.unsnoc (bare name) of
                      Just (parent, '|') -> (parent, False, T.words kids)
                      _ -> (bare name, True, T.words kids)
                 in entry : go (T.drop 1 after)
              _ -> (bare name, True, []) : go rest
    bare = T.dropWhileEnd (== ',')

-- | An exported name without the module that declared it.
nameOnly :: Text -> OpName
nameOnly t = OpName (maybe t snd (moduleOf t))

-- | Split a name into the module that declared it and the name itself.
moduleOf :: Text -> Maybe (Text, Text)
moduleOf = go []
  where
    go seen t = case component t of
      Just (c, rest) -> go (c : seen) rest
      Nothing
        | null seen -> Nothing
        | otherwise -> Just (T.intercalate "." (reverse seen), t)
    component t = do
      (c, _) <- T.uncons t
      if isUpper c
        then case T.break (== '.') t of
          (before, rest)
            | Just after <- T.stripPrefix "." rest,
              not (T.null before) ->
                Just (before, after)
          _ -> Nothing
        else Nothing
