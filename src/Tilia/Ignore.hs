{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Ignore files written the way @.gitignore@ files are.
module Tilia.Ignore
  ( IgnoreFile,
    parseIgnoreFile,
    isIgnored,
    isIgnoredInProject,
    ignoreFilesAbove,
    belowRoot,
  )
where

import Data.Char
  ( isAlpha,
    isAlphaNum,
    isControl,
    isDigit,
    isHexDigit,
    isLower,
    isPrint,
    isPunctuation,
    isSpace,
    isSymbol,
    isUpper,
  )
import Data.List (inits, isPrefixOf, isSuffixOf, stripPrefix, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (joinPath, normalise, splitDirectories, (</>))
import Tilia.Utils (quietly)

-- | The patterns of one ignore file, in the order they are written.
newtype IgnoreFile = IgnoreFile [Pattern]

-- | One line of an ignore file.
data Pattern = Pattern
  { -- | Whether it begins with @!@ and so re-includes what it matches.
    patNegated :: Bool,
    -- | Whether it ends with @/@ and so matches directories alone.
    patDirectoryOnly :: Bool,
    -- | Whether it holds a @/@ other than a trailing one, and so matches
    -- the path from the ignore file's directory rather than the last name.
    patAnchored :: Bool,
    -- | What it matches.
    patTokens :: [Token]
  }

-- | One piece of a pattern.
data Token
  = -- | A character that stands for itself.
    Literal Char
  | -- | @?@, any character but @/@.
    AnyChar
  | -- | @*@, any run of characters without @/@.
    AnyRun
  | -- | @**/@ at the start, any number of leading directories.
    LeadingDirectories
  | -- | @/**/@, a @/@ with any number of directories inside it.
    InnerDirectories
  | -- | @/**@ at the end, everything below the directory before it.
    Everything
  | -- | A bracket expression, whether it is negated, and what it holds.
    Bracket Bool [Char -> Bool]

-- | Read an ignore file.
parseIgnoreFile :: Text -> IgnoreFile
parseIgnoreFile =
  IgnoreFile
    . mapMaybe (parsePattern . T.unpack . withoutReturn)
    . T.lines
  where
    withoutReturn = T.dropWhileEnd (== '\r')

-- | Read one line of an ignore file.
parsePattern :: String -> Maybe Pattern
parsePattern line0 = case trimmed line0 of
  "" -> Nothing
  '#' : _ -> Nothing
  '!' : rest -> pattern True rest
  line -> pattern False line
  where
    pattern negated body = do
      let directoryOnly = "/" `isSuffixOf` body
          stripped = if directoryOnly then init body else body
          anchored = '/' `elem` stripped
          relative = case stripped of
            '/' : rest -> rest
            _ -> stripped
      tokens <- tokenize relative
      if null tokens
        then Nothing
        else
          Just
            Pattern
              { patNegated = negated,
                patDirectoryOnly = directoryOnly,
                patAnchored = anchored,
                patTokens = tokens
              }

-- | A line without the spaces it ends in, except one escaped with @\\@.
trimmed :: String -> String
trimmed = reverse . go . reverse
  where
    go = \case
      ' ' : '\\' : rest -> ' ' : '\\' : rest
      ' ' : rest -> go rest
      other -> other

-- | Split a pattern into its pieces, or 'Nothing' where a bracket is left
-- open.
tokenize :: String -> Maybe [Token]
tokenize = go True
  where
    go atStart = \case
      [] -> Just []
      '*' : '*' : '/' : rest
        | atStart -> (LeadingDirectories :) <$> go True rest
      '/' : '*' : '*' : '/' : rest -> (InnerDirectories :) <$> go True rest
      "/**" -> Just [Everything]
      '*' : rest -> (AnyRun :) <$> go False (dropWhile (== '*') rest)
      '?' : rest -> (AnyChar :) <$> go False rest
      '[' : rest -> do
        (token, rest') <- bracket rest
        (token :) <$> go False rest'
      '\\' : c : rest -> (Literal c :) <$> go False rest
      c : rest -> (Literal c :) <$> go (c == '/') rest

-- | Read a bracket expression after its @[@.
bracket :: String -> Maybe (Token, String)
bracket = \case
  c : rest | c `elem` ['!', '^'] -> members True [] True rest
  rest -> members False [] True rest
  where
    members negated acc first = \case
      ']' : rest | not first -> Just (Bracket negated acc, rest)
      '[' : ':' : rest
        | (name, ':' : ']' : rest') <- break (== ':') rest,
          Just p <- lookup name posixClasses ->
            members negated (p : acc) False rest'
      '\\' : c : rest -> range negated acc c rest
      c : rest -> range negated acc c rest
      [] -> Nothing
    range negated acc lo = \case
      '-' : hi : rest
        | hi /= ']' ->
            let hi' = case (hi, rest) of
                  ('\\', h : _) -> h
                  _ -> hi
                rest' = if hi == '\\' then drop 1 rest else rest
             in members negated ((\c -> lo <= c && c <= hi') : acc) False rest'
      rest -> members negated ((== lo) : acc) False rest

-- | The character classes a bracket expression can name.
posixClasses :: [(String, Char -> Bool)]
posixClasses =
  [ ("alnum", isAlphaNum),
    ("alpha", isAlpha),
    ("blank", (`elem` [' ', '\t'])),
    ("cntrl", isControl),
    ("digit", isDigit),
    ("graph", \c -> isPrint c && not (isSpace c)),
    ("lower", isLower),
    ("print", isPrint),
    ("punct", \c -> isPunctuation c || isSymbol c),
    ("space", isSpace),
    ("upper", isUpper),
    ("xdigit", isHexDigit)
  ]

-- | Whether a pattern matches a path, given relative to the directory of
-- the ignore file the pattern is in, and whether that path is a directory.
matches :: Pattern -> [String] -> Bool -> Bool
matches p segments isDirectory
  | patDirectoryOnly p && not isDirectory = False
  | patAnchored p = wildmatch (patTokens p) (joinSegments segments)
  | otherwise = case reverse segments of
      name : _ -> wildmatch (patTokens p) name
      [] -> False

-- | Whether the pieces of a pattern match a path, @/@ and all.
wildmatch :: [Token] -> String -> Bool
wildmatch = \case
  [] -> null
  Literal c : ts -> \case
    x : xs | x == c -> wildmatch ts xs
    _ -> False
  AnyChar : ts -> \case
    x : xs | x /= '/' -> wildmatch ts xs
    _ -> False
  Bracket negated ps : ts -> \case
    x : xs | x /= '/', any ($ x) ps /= negated -> wildmatch ts xs
    _ -> False
  AnyRun : ts -> \s ->
    any
      (wildmatch ts)
      [rest | (skipped, rest) <- splits s, '/' `notElem` skipped]
  LeadingDirectories : ts -> \s ->
    any
      (wildmatch ts)
      (s : [rest | (skipped, rest) <- splits s, "/" `isSuffixOf` skipped])
  InnerDirectories : ts -> \s ->
    any
      (wildmatch ts)
      [ rest
      | (skipped, rest) <- splits s,
        "/" `isPrefixOf` skipped,
        "/" `isSuffixOf` skipped
      ]
  Everything : _ -> \s -> "/" `isPrefixOf` s && length s > 1
  where
    splits s = zip (inits s) (tails s)

-- | A path's segments, joined with @/@.
joinSegments :: [String] -> String
joinSegments = \case
  [] -> ""
  s : ss -> s <> concatMap ('/' :) ss

-- | Whether a file is ignored, given its path as segments relative to the
-- project root and the ignore files found in the directories above it, each
-- by its directory's segments.
isIgnored :: Map [String] IgnoreFile -> [String] -> Bool
isIgnored files path =
  any (\n -> excluded (take n path) True) [1 .. length path - 1]
    || excluded path False
  where
    excluded entry isDirectory =
      case [ not (patNegated p)
           | (n, IgnoreFile ps) <- applicable entry,
             p <- ps,
             matches p (drop n entry) isDirectory
           ] of
        [] -> False
        verdicts -> last verdicts
    applicable entry =
      [ (n, file)
      | n <- [0 .. length entry - 1],
        Just file <- [Map.lookup (take n entry) files]
      ]

-- | Whether the @.tiliaignore@ files of a project ignore a file, which need
-- not exist.
isIgnoredInProject ::
  -- | The project root, canonical.
  FilePath ->
  -- | The file.
  FilePath ->
  IO Bool
isIgnoredInProject root path = quietly False $ do
  file <- canonicalizePath path
  case belowRoot root file of
    Nothing -> pure False
    Just below -> do
      ignoreFiles <- ignoreFilesAbove root [below]
      pure (isIgnored ignoreFiles below)

-- | The @.tiliaignore@ files in the project root and in every directory
-- between it and the given files, each by its directory's segments relative
-- to the root.
ignoreFilesAbove :: FilePath -> [[FilePath]] -> IO (Map [FilePath] IgnoreFile)
ignoreFilesAbove root paths =
  Map.fromList . concat <$> traverse read' (Set.toList directories)
  where
    directories =
      Set.fromList [take n path | path <- paths, n <- [0 .. length path - 1]]
    read' directory = do
      let file = joinPath (root : directory) </> ".tiliaignore"
      exists <- doesFileExist file
      if exists
        then (\t -> [(directory, parseIgnoreFile t)]) <$> T.readFile file
        else pure []

-- | A path's segments relative to the project root, if it is under it.
belowRoot ::
  -- | The project root.
  FilePath ->
  -- | The path.
  FilePath ->
  Maybe [FilePath]
belowRoot root path =
  stripPrefix (splitDirectories root) (splitDirectories (normalise path))
