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
    directivePresentOnLine,
    directiveOnLine,
    blankBelow,
    closesABranch,
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

-- | Does this line hold a preprocessor directive?
directivePresentOnLine :: Int -> Lines -> Bool
directivePresentOnLine n ls = isJust (directiveOnLine =<< lineAt n ls)

-- | The keyword a directive line opens with, and everything after its @#@.
directiveOnLine :: Text -> Maybe (Text, Text)
directiveOnLine l = case T.uncons (T.stripStart l) of
  Just ('#', rest)
    | not (T.null keyword) -> Just (keyword, body)
    where
      body = T.stripStart rest
      keyword = T.takeWhile isAsciiLower body
  _ -> Nothing

-- | Did the author leave an empty line below this line?
blankBelow :: Int -> Lines -> Bool
blankBelow start ls = go (start + 1)
  where
    go n
      | n > IntMap.size (lnWritten ls) = False
      | Nothing <- lineAt n ls = go (n + 1)
      | leadsOut n = go (n + 1)
      | otherwise = blankAt n ls
    leadsOut n = case directiveOnLine =<< lineAt n ls of
      Just (keyword, _) -> keyword `elem` leavingKeywords
      Nothing -> False

-- | Does the empty line under this one stand at the end of a branch?
closesABranch :: Int -> Lines -> Bool
closesABranch n ls = go False (n + 1)
  where
    go crossed k
      | k > IntMap.size (lnWritten ls) = False
      | otherwise = case lineAt k ls of
          Nothing -> go crossed (k + 1)
          Just l
            | T.null (T.strip l) -> go True (k + 1)
            | Just (keyword, _) <- directiveOnLine l ->
                crossed && keyword `elem` leavingKeywords
            | otherwise -> False

-- | The directives that lead out of the region the line below them is in,
-- rather than into one it is not.
leavingKeywords :: [Text]
leavingKeywords = ["elif", "elifdef", "elifndef", "else", "endif"]
