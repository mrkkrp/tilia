{-# LANGUAGE OverloadedStrings #-}

-- | Interface files read directly, against what @ghc --show-iface@ makes of
-- them, for every module of every package this project is built against.
module Tilia.Fixity.HiFileSpec (spec) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Test.Hspec
import Tilia.Fixity.HiFile (decodeHiFile)
import Tilia.Fixity.Interface (Interface (..), fromHiFile, parseInterface)
import Tilia.Fixity.PackageDb (readInstalledPackages)
import Tilia.Fixity.Plan (BuildPlan)
import Tilia.Process (readProgramOutput)
import Tilia.Utils (inParallel)
import Tilia.WithProjectPlan
  ( Dependency (..),
    dependenciesOf,
    testsFor,
    withProjectPlan,
  )

spec :: Spec
spec = withProjectPlan withPlan

withPlan :: BuildPlan -> Spec
withPlan plan = do
  installed <- runIO readInstalledPackages
  dependencies <- runIO (dependenciesOf (const True) plan installed)
  let modules = concatMap depModules dependencies
  readings <- runIO (Map.fromList <$> inParallel reading modules)
  describe "interface files read directly" $ do
    it "are there to read" $
      length modules `shouldSatisfy` (>= 500)

    it "decode, every one of them" $
      [(m, why) | (m, path) <- modules, Just (Left why, _) <- [Map.lookup path readings]]
        `shouldBe` []

    for_ (concatMap testsFor dependencies) $ \(label, chunk) ->
      it ("agree with ghc --show-iface on " <> label) $
        [ (m, disagreement)
        | (m, path) <- chunk,
          Just (Right (Right direct), Just shown) <- [Map.lookup path readings],
          disagreement <- disagreements m direct shown
        ]
          `shouldBe` []
  where
    reading (m, path) = do
      bytes <- BS.readFile path
      shown <- maybe Nothing (parseInterface m) <$> readProgramOutput "ghc" ["--show-iface", path]
      pure (path, (fromHiFile m <$> decodeHiFile bytes, shown))

-- | Where a direct reading and the text disagree beyond what the text
-- cannot tell.
disagreements :: Text -> Interface -> Interface -> [Text]
disagreements m direct shown =
  [ "fixities"
  | not (interfaceDeclares shown `Map.isSubmapOf` interfaceDeclares direct)
      || byOperator (interfaceDeclares direct) /= byOperator (interfaceDeclares shown)
  ]
    <> [ "re-exports"
       | Set.fromList (interfaceReexports direct)
           /= Set.fromList [r | r@(from, _) <- interfaceReexports shown, from /= m]
       ]
    <> ["children" | interfaceChildren direct /= interfaceChildren shown]
  where
    byOperator = Set.fromList . fmap (\((_, op), fixity) -> (op, show fixity)) . Map.toList
