{-# LANGUAGE OverloadedStrings #-}

-- | Formatting whole modules.
module Tilia.RenderSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as T
import Test.Hspec
import Tilia.Doc (printDoc)
import Tilia.Render
import Tilia.TestUtils (parsed)

spec :: Spec
spec =
  -- Without fixities nothing is asserted about how a chain associates, so
  -- nothing is rearranged and every operator is treated alike.
  describe "operator chains"
    $ it "leaves a chain alone when the fixities are unknown"
    $ format
      [ "module M where",
        "",
        "f =",
        "  a * b",
        "    + c * d"
      ]
      `shouldBe` [ "module M where",
                   "",
                   "f =",
                   "  a",
                   "    * b",
                   "    + c",
                   "    * d"
                 ]

-- | Format the given lines with no fixities known and give the result back
-- as lines.
format :: [Text] -> [Text]
format =
  T.lines
    . printDoc
    . renderModule defaultRenderConfig
    . parsed
    . T.unlines
