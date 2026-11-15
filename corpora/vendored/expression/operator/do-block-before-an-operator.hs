module Spec where

spec :: IO ()
spec = do
  it "adds" $ do
    sum $ do
      [1, 2]
    `shouldBe` 3

  it "adds, written broken" $
    do
      sum $ do
        [1, 2]
      `shouldBe` 3

recovered :: IO ()
recovered = when ready $ do
  run
  `catch` handler
