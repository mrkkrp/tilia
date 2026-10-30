{-# LANGUAGE OverloadedStrings #-}

-- | The lines of a module, and the questions that can be asked of them
-- without a parse.
module Tilia.Source.Lines
  ( -- * The lines
    Written (..),
    Lines,
    linesOf,
    dropping,
    lineTexts,
    lineAt,
    blankAt,
    blankAcross,
    offsetOf,
    columnOf,
    contentColumn,
    directivePresentOnLine,
    directiveOnLine,
    conditionalKeywords,
    opensGroup,
    continuesGroup,
    opaqueKeywords,
  )
where

import Data.Char (isAsciiLower, isSpace)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T

-- | The text of a module as its author wrote it.
--
-- Distinguished from the text handed to the parser because the two are the
-- same only when the preprocessor is not involved.
newtype Written = Written Text
  deriving (Eq, Show)

-- | The lines of a source, numbered from one as the compiler numbers them,
-- but also the lines that a particular CPP configuration does not contain.
data Lines = Lines
  { -- | Every line of the module as written.
    lnWritten :: !(IntMap Text),
    -- | The ones this configuration does not contain.
    lnDropped :: !IntSet
  }

-- | Read the lines of a module, every one of which it has.
linesOf :: Written -> Lines
linesOf (Written text) =
  Lines
    { lnWritten = IntMap.fromList (zip [1 ..] (T.lines text)),
      lnDropped = IntSet.empty
    }

-- | Drop the given ranges from the 'Lines'.
dropping :: [(Int, Int)] -> Lines -> Lines
dropping ranges ls =
  ls
    { lnDropped =
        IntSet.union
          (lnDropped ls)
          (IntSet.fromList (concat [[from .. to] | (from, to) <- ranges]))
    }

-- | Every line of the module as written, in order, whatever this
-- configuration has of them.
lineTexts :: Lines -> [Text]
lineTexts = IntMap.elems . lnWritten

-- | The text of a line, if this configuration of the module has one.
lineAt :: Int -> Lines -> Maybe Text
lineAt n ls
  | IntSet.member n (lnDropped ls) = Nothing
  | otherwise = IntMap.lookup n (lnWritten ls)

-- | Was this line empty?
blankAt :: Int -> Lines -> Bool
blankAt n = maybe False (T.all isSpace) . lineAt n

-- | Were all the lines from one to another, both included, empty?
blankAcross :: (Int, Int) -> Lines -> Bool
blankAcross (from, to) ls = all (`blankAt` ls) [from .. to]

-- | How many characters of a line come before the compiler's column.
--
-- A column is not a character offset. The lexer counts a tab as advancing to
-- the next multiple of eight, so a line with a tab in it has more columns
-- than it has characters, and cutting the text at a column would cut in the
-- wrong place. In a file indented with tabs that is every line.
offsetOf :: Text -> Int -> Int
offsetOf line column = T.foldr walk (\i !_ -> i) line 0 1
  where
    walk ch next i c
      | c < column = next (i + 1) (afterChar ch c)
      | otherwise = i

-- | The compiler's column for the character at this offset.
columnOf :: Text -> Int -> Int
columnOf line offset = T.foldl' (flip afterChar) 1 (T.take offset line)

-- | The compiler's column for the first character of a line that is not
-- white space.
contentColumn :: Text -> Int
contentColumn l = columnOf l (T.length (T.takeWhile isSpace l))

-- | Where the column moves to once this character has been read.
afterChar :: Char -> Int -> Int
afterChar ch c
  | ch == '\t' = ((c - 1) `div` 8 + 1) * 8 + 1
  | otherwise = c + 1

-- | Does this line hold a preprocessor directive?
directivePresentOnLine :: Int -> Lines -> Bool
directivePresentOnLine n ls = isJust (directiveOnLine =<< lineAt n ls)

-- | The keyword a directive line opens with, and everything after its @#@.
directiveOnLine :: Text -> Maybe (Text, Text)
directiveOnLine l = case T.uncons (T.stripStart l) of
  Just ('#', rest)
    | keyword `elem` directiveKeywords -> Just (keyword, body)
    where
      body = T.stripStart rest
      keyword = T.takeWhile isAsciiLower body
  _ -> Nothing

-- | Every directive the C preprocessor takes, whether or not this module
-- can do anything with the ones it names.
directiveKeywords :: [Text]
directiveKeywords = conditionalKeywords <> opaqueKeywords

-- | The directives that ask a question, and so split a module in two.
conditionalKeywords :: [Text]
conditionalKeywords = opensGroup <> continuesGroup <> ["endif"]

-- | The keywords that open a group or continue one.
opensGroup, continuesGroup :: [Text]
opensGroup = ["if", "ifdef", "ifndef"]
continuesGroup = ["elif", "elifdef", "elifndef", "else"]

-- | The unconditional directives that do not split the source code.
opaqueKeywords :: [Text]
opaqueKeywords =
  ["define", "undef", "include", "line", "error", "warning", "pragma"]
