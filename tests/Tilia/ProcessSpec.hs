{-# LANGUAGE OverloadedStrings #-}

-- | Running a program and collecting its output.
module Tilia.ProcessSpec (spec) where

import Data.Text qualified as T
import System.Timeout (timeout)
import Test.Hspec

import Tilia.Process (readProgramOutput)

spec :: Spec
spec = do
  describe "what a program printed" $ do
    it "comes back as it was printed" $
      shell "printf 'one\\ntwo\\n'" `shouldReturn` Just "one\ntwo\n"

    it "is read as UTF-8, whatever the machine's locale is" $
      shell "printf 'ma\\303\\257ntainer\\n'" `shouldReturn` Just "maïntainer\n"

    it "survives bytes that are not UTF-8 at all" $
      (fmap survived <$> shell "printf 'before\\377after'")
        `shouldReturn` Just True

    it "ends its lines with newlines however the program ended them" $
      shell "printf 'a\\r\\nb\\r\\n'" `shouldReturn` Just "a\nb\n"

  describe "a program that has nothing to say" $ do
    it "is nothing when it failed" $
      shell "printf 'half an answer'; exit 3" `shouldReturn` Nothing

    it "is nothing when it is not there to run" $
      readProgramOutput "tilia-no-such-program-exists" [] `shouldReturn` Nothing

    it "is nothing rather than a crash when it is a directory" $
      readProgramOutput "." [] `shouldReturn` Nothing

  describe "a program that says a great deal on its error stream" $
    it "is read to the end all the same" $ do
      answered <-
        timeout (30 * 1000000) $
          shell "yes error | head -c 200000 >&2; printf 'the answer'"
      answered `shouldBe` Just (Just "the answer")

-- | Run a shell command and take what it printed.
shell :: String -> IO (Maybe T.Text)
shell command = readProgramOutput "sh" ["-c", command]

-- | Did what was printed either side of the unreadable byte come through?
survived :: T.Text -> Bool
survived out = T.isInfixOf "before" out && T.isInfixOf "after" out
