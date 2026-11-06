{-# LANGUAGE OverloadedStrings #-}

-- | The settings a project writes in its @tilia.toml@.
module Tilia.SettingsSpec (spec) where

import Data.Text (Text)
import Test.Hspec

import Tilia.Settings
import Tilia.TestUtils (withFiles)

spec :: Spec
spec = do
  describe "a settings file" $ do
    it "that is empty sets nothing" $
      parseSettings "" `shouldBe` Right defaultSettings

    it "that holds only comments and empty lines sets nothing" $
      parseSettings "# Nothing here.\n\n   # Nor here.\n"
        `shouldBe` Right defaultSettings

    it "sets the column limit" $
      "column-limit = 100\n" `limitsAt` 100

    it "reads a key and a value however they are spaced" $ do
      "column-limit=100" `limitsAt` 100
      "   column-limit   =   90   " `limitsAt` 90

    it "reads a comment after a value" $
      "column-limit = 100 # a hundred columns" `limitsAt` 100

    it "reads a value with a sign" $
      "column-limit = +100" `limitsAt` 100

    it "sets leading commas" $ do
      parseSettings "leading-commas = true\n"
        `shouldBe` Right defaultSettings{settingsCommaStyle = LeadingCommas}
      parseSettings "leading-commas = false\n"
        `shouldBe` Right defaultSettings{settingsCommaStyle = TrailingCommas}

    it "sets several settings" $
      parseSettings "column-limit = 100\nleading-commas = true\n"
        `shouldBe` Right alternativeSettings

  describe "a settings file is refused where" $ do
    it "the column limit is not a whole number greater than 0" $ do
      let why = "column-limit is a whole number greater than 0"
      "column-limit = 0" `fails` (1, why)
      "column-limit = -80" `fails` (1, why)
      "column-limit = true" `fails` (1, why)
      "column-limit = 99999999999999999999" `fails` (1, why)

    it "leading commas are neither true nor false" $
      "leading-commas = 1" `fails` (1, "leading-commas is true or false")

    it "a value is a string" $
      "column-limit = \"100\""
        `fails` (1, "a value is true, false or a whole number")

    it "a number begins with a zero" $
      "column-limit = 0100"
        `fails` (1, "a value is true, false or a whole number")

    it "a setting is given twice" $
      "column-limit = 100\n\ncolumn-limit = 90\n"
        `fails` (3, "column-limit is already set on line 1")

    it "a setting is unknown" $
      "# A comment.\nwidth = 80\n" `fails` (2, "there is no setting width")

    it "a line is not key = value" $ do
      "[tilia]" `fails` (1, "expected a line of key = value")
      "column-limit" `fails` (1, "expected a line of key = value")
      "\"column-limit\" = 100" `fails` (1, "expected a line of key = value")

  describe "reading a project's settings" $ do
    it "gives the defaults where the project has no settings file" $
      withFiles [] $ \root ->
        readSettings root `shouldReturn` Right defaultSettings

    it "reads the project's settings file" $
      withFiles [(settingsFile, "column-limit = 100\n")] $ \root ->
        readSettings root
          `shouldReturn` Right defaultSettings{settingsColumnLimit = 100}

-- | Check that a settings file sets the column limit.
limitsAt ::
  -- | The file's text.
  Text ->
  -- | The column limit.
  Int ->
  Expectation
limitsAt written limit =
  parseSettings written
    `shouldBe` Right defaultSettings{settingsColumnLimit = limit}

-- | Check that a settings file is refused at a line, for a reason.
fails ::
  -- | The file's text.
  Text ->
  -- | The number of the line, and why.
  (Int, Text) ->
  Expectation
fails written (n, why) =
  parseSettings written `shouldBe` Left (SettingsMalformed n why)
