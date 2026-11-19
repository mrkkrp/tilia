{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reading a module as cpp, in the traditional mode GHC runs it in, hands
-- it to GHC.
module Tilia.Cpp.Expansion
  ( cppExpansion,
    hashLines,
    changedByCpp,
    joinedTo,
    endsInBackslash,
    Macro (..),
    definition,
  )
where

import Data.Char (isAsciiLower, isDigit, isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Tilia.Cpp.Macros (isNameChar, isNameStart)
import Tilia.Source.Lines (conditionalKeywords, directiveOnLine, isDirective)

-- | A configuration as cpp hands it to GHC: lines ending in a backslash
-- joined to the next, directives blanked, and the macros it defines
-- expanded, each line keeping its number.
--
-- cpp reads a quote as running to the next one or to the end of the line
-- and expands nothing inside it, so a macro after a primed name on its line
-- is not expanded.
cppExpansion ::
  -- | The configuration as written.
  Text ->
  Text
cppExpansion = T.unlines . go Map.empty . spliced . T.lines
  where
    go macros = \case
      [] -> []
      l : ls
        | Just (keyword, body) <- directiveOnLine l ->
            "" : go (defining keyword body macros) ls
        | Map.null macros -> l : go macros ls
        | otherwise ->
            let (pieces, taken, rest) = scan macros Set.empty l ls
             in T.concat pieces : replicate taken "" <> go macros rest

-- | The lines cpp acts on but for the directives of conditionals, and the
-- lines GHC reads past, such as line markers, with lines joined as cpp
-- joins them and spaces after the hash and at the end left out.
--
-- cpp passes any other line beginning with a hash on to GHC as it is.
hashLines ::
  -- | The module.
  Text ->
  [Text]
hashLines = mapMaybe hashed . spliced . T.lines
  where
    hashed l = do
      rest <- T.stripStart <$> T.stripPrefix "#" l
      case directiveOnLine l of
        Just (keyword, _) | keyword `notElem` conditionalKeywords -> pure ()
        Nothing | Just (c, _) <- T.uncons rest, c == '!' || isDigit c -> pure ()
        _ -> Nothing
      pure ("#" <> T.stripEnd rest)

-- | Does cpp change a module before GHC reads it: is a line a directive,
-- or one it joins to the next?
changedByCpp ::
  -- | The module.
  Text ->
  Bool
changedByCpp = any (\l -> isDirective l || endsInBackslash l) . T.lines

-- | Lines with the backslash ending each joined to the next, as cpp joins
-- them, and an empty line in place of each line joined on.
spliced ::
  -- | The lines as written.
  [Text] ->
  [Text]
spliced = \case
  [] -> []
  l : ls -> case joinedTo l ls of
    (continued, rest) ->
      T.concat (cut l continued) : ("" <$ continued) <> spliced rest
  where
    cut l = \case
      [] -> [l]
      n : ns -> T.dropEnd 1 (T.stripEnd l) : cut n ns

-- | The lines cpp joins to a line, each because the line before it ends in
-- a backslash, and the lines after them.
joinedTo ::
  -- | The line.
  Text ->
  -- | The lines after it.
  [Text] ->
  ([Text], [Text])
joinedTo l = \case
  n : ls | endsInBackslash l -> case joinedTo n ls of
    (more, rest) -> (n : more, rest)
  ls -> ([], ls)

-- | Does cpp join the line after this one to it?
endsInBackslash ::
  -- | The line.
  Text ->
  Bool
endsInBackslash l
  | T.null l = False
  | otherwise = case T.last l of
      '\\' -> True
      c -> isSpace c && endsInBackslash (T.init l)

-- | A macro: its parameters, unless it is object-like, and its body.
data Macro = Macro (Maybe [Text]) Text

-- | The macros once a directive is read.
defining ::
  -- | The directive's keyword.
  Text ->
  -- | What follows its hash.
  Text ->
  Map Text Macro ->
  Map Text Macro
defining keyword body macros = case keyword of
  "define"
    | Just (name, macro) <- definition body -> Map.insert name macro macros
  "undef" -> Map.delete (fst (named body)) macros
  _ -> macros

-- | The name a @#define@ gives a macro, and the macro.
definition ::
  -- | What follows the directive's hash.
  Text ->
  Maybe (Text, Macro)
definition body = case named body of
  (name, rest)
    | T.null name -> Nothing
    | otherwise -> Just (name, macroOf rest)
  where
    macroOf rest = case T.stripPrefix "(" rest of
      Just inside
        | (written, after) <- T.breakOn ")" inside,
          not (T.null after) ->
            Macro
              (Just [p | p <- fmap T.strip (T.splitOn "," written), p /= ""])
              (bodyOf (T.drop 1 after))
      _ -> Macro Nothing (bodyOf rest)
    bodyOf = T.strip . uncommented
    uncommented t = case T.breakOn "/*" t of
      (before, after)
        | T.null after -> t
        | otherwise ->
            before <> uncommented (T.drop 2 (snd (T.breakOn "*/" after)))

-- | The name a directive is about, and what follows it.
named ::
  -- | What follows the directive's hash.
  Text ->
  (Text, Text)
named = T.span isNameChar . T.stripStart . T.dropWhile isAsciiLower

-- | Expand the macros used on a line, taking the lines after it that the
-- arguments of a use go on to: the pieces of the line, how many lines it
-- took, and the lines left.
scan ::
  Map Text Macro ->
  -- | The macros not to expand again, being expanded already.
  Set Text ->
  -- | The line.
  Text ->
  -- | The lines after it.
  [Text] ->
  ([Text], Int, [Text])
scan macros off t ls = case T.uncons t of
  Nothing -> ([], 0, ls)
  Just (c, _)
    | isQuote c ->
        let (q, after) = quoted t in q `before` scan macros off after ls
    | isNameStart c,
      (name, after) <- T.span isNameChar t ->
        case Map.lookup name macros of
          Just (Macro params body)
            | Set.notMember name off,
              Just (bound, after', taken, ls') <- usedWith params after ls ->
                let expanded =
                      rescanned (Set.insert name off) (substituted bound body)
                    (pieces, more, ls'') = scan macros off after' ls'
                 in (expanded : pieces, taken + more, ls'')
          _ -> name `before` scan macros off after ls
    | otherwise ->
        let (plain, after) = T.break special t
         in plain `before` scan macros off after ls
  where
    before x (xs, n, rest) = (x : xs, n, rest)
    special ch = isQuote ch || isNameStart ch
    rescanned off' text =
      let (pieces, _, _) = scan macros off' text [] in T.concat pieces

-- | The parameters of a use bound to its arguments, what follows the use,
-- how many lines after it the use took, and the lines left; 'Nothing' where
-- a function-like macro is named without arguments.
usedWith ::
  -- | The parameters, unless the macro is object-like.
  Maybe [Text] ->
  -- | What follows the name.
  Text ->
  -- | The lines after it.
  [Text] ->
  Maybe ([(Text, Text)], Text, Int, [Text])
usedWith Nothing after ls = Just ([], after, 0, ls)
usedWith (Just params) after ls = do
  (opened, skipped, ls') <- opening (T.stripStart after) 0 ls
  (args, after', taken, ls'') <- arguments opened ls'
  case (params, args) of
    ([], [a]) | T.all isSpace a -> Just ([], after', skipped + taken, ls'')
    _
      | length params == length args ->
          Just (zip params args, after', skipped + taken, ls'')
      | otherwise -> Nothing
  where
    opening t n rest = case T.uncons t of
      Just ('(', inside) -> Just (inside, n, rest)
      Nothing | l : more <- rest -> opening (T.stripStart l) (n + 1) more
      _ -> Nothing

-- | The arguments of a use from after its opening bracket, split at the
-- commas outside brackets, what follows its closing bracket, how many lines
-- after it they took, and the lines left.
arguments ::
  -- | The rest of the line after the opening bracket.
  Text ->
  -- | The lines after it.
  [Text] ->
  Maybe ([Text], Text, Int, [Text])
arguments = go (0 :: Int) [] [] 0
  where
    go depth done current taken t ls =
      let (plain, rest) = T.break (`elem` ("(),\"'" :: String)) t
          current' = plain : current
          arg = T.concat (reverse current')
       in case T.uncons rest of
            Nothing -> case ls of
              l : more -> go depth done (" " : current') (taken + 1) l more
              [] -> Nothing
            Just (c, after)
              | isQuote c ->
                  let (q, after') = quoted rest
                   in go depth done (q : current') taken after' ls
              | c == ')',
                depth == 0 ->
                  Just (reverse (arg : done), after, taken, ls)
              | c == ',', depth == 0 -> go depth (arg : done) [] taken after ls
              | otherwise ->
                  go
                    (depth + if c == '(' then 1 else if c == ')' then -1 else 0)
                    done
                    (T.singleton c : current')
                    taken
                    after
                    ls

-- | A body with its parameters replaced by the arguments bound to them,
-- inside quotes too.
substituted ::
  -- | Each parameter with its argument.
  [(Text, Text)] ->
  -- | The body.
  Text ->
  Text
substituted [] = id
substituted bound = T.concat . go
  where
    go t = case T.uncons t of
      Nothing -> []
      Just (c, _)
        | isNameStart c,
          (name, rest) <- T.span isNameChar t ->
            fromMaybe name (lookup name bound) : go rest
        | otherwise ->
            let (plain, rest) = T.break isNameStart t in plain : go rest

-- | Does a quote, as cpp reads quotes, begin with this character?
isQuote ::
  -- | The character.
  Char ->
  Bool
isQuote c = c == '"' || c == '\''

-- | The quote a text begins with, up to its closing quote or the end of the
-- line, and what follows it.
quoted ::
  -- | The text, beginning with a quote.
  Text ->
  (Text, Text)
quoted t = case T.uncons t of
  Just (q, inside) -> T.splitAt (1 + closing q inside) t
  Nothing -> (t, t)
  where
    closing q s = case T.break (\c -> c == q || c == '\\') s of
      (plain, rest) -> case T.uncons rest of
        Just ('\\', escaped) ->
          T.length plain
            + 1
            + T.length (T.take 1 escaped)
            + closing q (T.drop 1 escaped)
        Just _ -> T.length plain + 1
        Nothing -> T.length plain
