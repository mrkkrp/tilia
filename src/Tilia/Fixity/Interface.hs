{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading a module's operators out of interface files.
module Tilia.Fixity.Interface
  ( Interface (..),
    readInterface,
    parseInterface,
  )
where

import Data.Char (isUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Tilia.Fixity
import Tilia.Process (readProgramOutput)
import Tilia.Utils (quietly)

-- | What an interface says about the operators a module offers.
data Interface = Interface
  { -- | The fixities the module declares itself, by the namespace each
    -- governs.
    interfaceDeclares :: Fixities,
    -- | The names it reexports, each with the source module.
    interfaceReexports :: [(Text, OpName)],
    -- | What it exports under each name. This is what @T(..)@ in an import
    -- list stands for.
    interfaceChildren :: Map OpName (Set OpName)
  }
  deriving (Eq, Show)

-- | Read a module's interface file.
readInterface ::
  -- | The module the file is supposed to hold.
  Text ->
  -- | The file.
  FilePath ->
  IO (Maybe Interface)
readInterface modName path =
  quietly Nothing $
    readProgramOutput "ghc" ["--show-iface", path] >>= \case
      Nothing -> pure Nothing
      Just out -> pure (parseInterface modName out)

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
                (Map.fromList (concatMap declared (sectionsNamed "fixities"))),
            interfaceReexports = concatMap reexports (sectionsNamed "exports:"),
            interfaceChildren =
              Map.unionsWith Set.union (map childrenIn (sectionsNamed "exports:"))
          }
  where
    holdsModule l = case T.words l of
      ("interface" : m : _) -> m == modName
      _ -> False
    sectionsNamed name = [body | (heading, body) <- sections out, heading == name]
    declared = mapMaybe fixityEntry . T.splitOn ","
    reexports = concatMap reexportsIn . T.words
    childrenIn section =
      Map.fromListWith
        Set.union
        [ (nameOnly parent, Set.fromList (map nameOnly kids))
        | (parent, kids@(_ : _)) <- exportEntries section
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
  where
    indented l = maybe False (== ' ') (fst <$> T.uncons l)

-- | Sort declared fixities into the namespaces they govern.
namespaced :: Set OpName -> Map OpName Fixity -> Fixities
namespaced types declared =
  Map.fromList
    [ ((namespace, op), fixity)
    | (op, fixity) <- Map.toList declared,
      namespace <- if Set.member op types then [InTypes] else [InTerms]
    ]

-- | Split the output into sections.
sections :: Text -> [(Text, Text)]
sections = go . T.lines
  where
    go = \case
      [] -> []
      (l : ls)
        | indented l -> go ls
        | otherwise ->
            let (body, rest) = span indented ls
                (heading, opening) = T.breakOn " " l
             in (heading, T.unwords (opening : body)) : go rest
    indented l = maybe False (== ' ') (fst <$> T.uncons l)

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
  where
    -- Not one digit: GHC gives @->@ a precedence of -1, below anything the
    -- report allows anyone to write, and drops it into a fixities line like
    -- any other.
    readPrecedence t = case T.signed T.decimal t of
      Right (p, rest) | T.null rest -> Just p
      _ -> Nothing

-- | Split an exports section into its entries, keeping the members an entry
-- wears in braces with the name they belong to.
exportEntries :: Text -> [(Text, [Text])]
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
                 in (bare name, T.words kids) : go (T.drop 1 after)
              _ -> (bare name, []) : go rest
    bare = T.dropWhileEnd (`elem` ("|," :: String))

-- | An exported name without the module that declared it.
nameOnly :: Text -> OpName
nameOnly t = OpName (maybe t snd (moduleOf t))

-- | The names an export entry reexports, with the module that declared
-- each.
reexportsIn :: Text -> [(Text, OpName)]
reexportsIn = mapMaybe qualified . T.split (`elem` ("{}|," :: String))
  where
    qualified name = case moduleOf name of
      Just (m, n) | not (T.null n) -> Just (m, OpName n)
      _ -> Nothing

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
