{-# LANGUAGE OverloadedStrings #-}

-- | Formatting whole modules.
module Tilia.RenderSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec

import Tilia.Doc (printDoc)
import Tilia.Render
import Tilia.Settings (Settings)
import Tilia.TestUtils (indentedBy, parsed, withEachSettings)

spec :: Spec
spec =
  -- Without fixities nothing is asserted about how a chain associates, so
  -- nothing is rearranged and every operator is treated alike.
  withEachSettings $ \settings ->
    describe "operator chains"
      $ it "leaves a chain alone when the fixities are unknown"
      $ format
        settings
        [ "module M where",
          "",
          "f =",
          "  a * b",
          "    + c * d"
        ]
        `shouldBe` [ "module M where",
                     "",
                     "f =",
                     indentedBy settings 1 <> "a",
                     indentedBy settings 2 <> "* b",
                     indentedBy settings 2 <> "+ c",
                     indentedBy settings 2 <> "* d"
                   ]

-- | Format the given lines with no fixities known and give the result back
-- as lines.
format :: Settings -> [Text] -> [Text]
format settings =
  T.lines
    . printDoc settings
    . renderModule defaultRenderConfig{rcSettings = settings}
    . parsed
    . T.unlines
