{-# LANGUAGE OverloadedStrings #-}

-- | Ignore files written the way @.gitignore@ files are.
module Tilia.IgnoreSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import System.FilePath (splitDirectories)
import Test.Hspec
import Tilia.Ignore

spec :: Spec
spec = do
  describe "a pattern" $ do
    it "without a slash matches a name at any depth" $
      "*.hs" `ignores` ["A.hs", "src/A.hs", "src/x/B.hs"] `butNot` ["A.hsc"]

    it "with a slash at the start or in the middle matches from the file's directory" $ do
      "/foo" `ignores` ["foo/a.hs"] `butNot` ["a/foo/b.hs"]
      "doc/*" `ignores` ["doc/x", "doc/y/x"] `butNot` ["a/doc/x"]

    it "with a slash at the end matches directories alone" $
      "foo/" `ignores` ["foo/a.hs", "a/foo/b.hs"] `butNot` ["foo", "a/foo"]

    it "with * and ? does not match a slash" $ do
      "a/*/b" `ignores` ["a/x/b"] `butNot` ["a/b", "a/x/y/b"]
      "?.hs" `ignores` ["a.hs"] `butNot` ["ab.hs"]

    it "with **/ at the start matches in every directory" $
      "**/gen/" `ignores` ["gen/G.hs", "q/gen/G.hs"] `butNot` ["gen"]

    it "with /**/ matches any number of directories, none included" $
      "a/**/b" `ignores` ["a/b", "a/x/b", "a/x/y/b"] `butNot` ["b"]

    it "with /** at the end matches everything inside" $
      "deep/**" `ignores` ["deep/1/2/3/f.hs"] `butNot` ["deep"]

    it "with a bracket matches the characters it lists, or all others" $ do
      "[ab].hs" `ignores` ["a.hs", "b.hs"] `butNot` ["c.hs"]
      "[!a].hs" `ignores` ["b.hs"] `butNot` ["a.hs"]
      "[^a].hs" `ignores` ["b.hs"] `butNot` ["a.hs"]
      "[a-c]x" `ignores` ["bx"] `butNot` ["dx"]
      "x[[:digit:]]" `ignores` ["x1"] `butNot` ["xa"]

    it "with an unclosed bracket matches nothing" $
      "[ab.hs" `ignores` [] `butNot` ["[ab.hs", "a.hs"]

    it "can escape what is special with a backslash" $ do
      "\\#x" `ignores` ["#x"] `butNot` []
      "\\!x" `ignores` ["!x"] `butNot` []
      "t/a\\*b" `ignores` ["t/a*b"] `butNot` ["t/axb"]
      "foo\\ " `ignores` ["foo "] `butNot` ["foo"]

    it "loses the spaces it ends in but not the ones it begins with" $ do
      "foo  " `ignores` ["foo"] `butNot` []
      " foo" `ignores` [" foo"] `butNot` ["foo"]

  describe "an ignore file" $ do
    it "says nothing on a blank line or a comment" $
      "# *.hs\n\n" `ignores` [] `butNot` ["A.hs"]

    it "lets a later pattern win, and ! re-include" $ do
      "*.hs\n!keep.hs\n" `ignores` ["a.hs"] `butNot` ["keep.hs"]
      "!keep.hs\n*.hs\n" `ignores` ["a.hs", "keep.hs"] `butNot` []

    it "cannot re-include a file whose directory it excluded" $ do
      "lib/\n!lib/keep.hs\n" `ignores` ["lib/keep.hs", "lib/drop.hs"] `butNot` []
      "lib/*\n!lib/keep.hs\n" `ignores` ["lib/drop.hs"] `butNot` ["lib/keep.hs"]

    it "reads lines ended with a carriage return" $
      "*.hs\r\n!keep.hs\r\n" `ignores` ["a.hs"] `butNot` ["keep.hs"]

  describe "ignore files in several directories" $ do
    it "match from their own directories" $
      nested [([], ""), (["src"], "/x/\n")] ["src/x/B.hs"] ["x/B.hs", "src/A.hs"]

    it "let the deeper file win" $
      nested [([], "*.hs\n"), (["src"], "!A.hs\n")] ["B.hs", "src/x/B.hs"] ["src/A.hs"]

    it "cannot re-include what a file above excluded the directory of" $
      nested [([], "q/\n"), (["q"], "!gen/\n")] ["q/gen/G.hs"] []

-- | Check which of the paths a single ignore file at the root ignores.
ignores :: Text -> [FilePath] -> [FilePath] -> Expectation
ignores content = nested [([], content)]

-- | Read the paths that follow as the ones the pattern should not ignore.
butNot :: ([FilePath] -> Expectation) -> [FilePath] -> Expectation
butNot = ($)

-- | Check which paths ignore files in several directories ignore.
nested :: [([FilePath], Text)] -> [FilePath] -> [FilePath] -> Expectation
nested files ignored kept =
  fmap (isIgnored rules . splitDirectories) (ignored <> kept)
    `shouldBe` fmap (const True) ignored <> fmap (const False) kept
  where
    rules = Map.fromList [(directory, parseIgnoreFile t) | (directory, t) <- files]
