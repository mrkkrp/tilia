{-# LANGUAGE OverloadedStrings #-}

-- | Interface files read directly, against what @ghc --show-iface@ makes of
-- them, for the modules of the packages this project is built against.
--
-- Every module is decoded, but only those that declare fixities and a fixed
-- eighth of the rest are checked against @ghc@, which costs a process per
-- module and was most of the suite's run time.
module Tilia.Fixity.HiFileSpec (spec) where

import Control.Exception (evaluate)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
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
  describe "interface files read directly" $ do
    it "are there to read" $
      length modules `shouldSatisfy` (>= 500)

    it "decode, every one of them" $ do
      failures <- concat <$> inParallel undecodable modules
      failures `shouldBe` []

    for_ (concatMap testsFor dependencies) $ \(label, chunk) ->
      it ("agree with ghc --show-iface on " <> label) $ do
        found <- concat <$> inParallel disagreeing chunk
        found `shouldBe` []
  where
    undecodable (m, path) = do
      bytes <- BS.readFile path
      evaluate [(m, why) | Left why <- [decodeHiFile bytes]]
    -- Forced before it is returned, so that the output of ghc, which is
    -- hundreds of megabytes across a run, is not kept alive until the end.
    disagreeing (m, path) = do
      direct <-
        either (const Nothing) (Just . fromHiFile m) . decodeHiFile
          <$> BS.readFile path
      case direct of
        Just (Right d)
          | not (Map.null (interfaceDeclares d)) || sampled m -> do
              shown <-
                maybe Nothing (parseInterface m)
                  <$> readProgramOutput "ghc" ["--show-iface", path]
              let found = [(m, x) | Just s <- [shown], x <- disagreements m d s]
              found <$ evaluate (length found)
        _ -> pure []

-- | Whether a module is in the fixed eighth of those that declare no
-- fixities and are checked against @ghc@ all the same.
--
-- Chosen by an FNV-1a hash of the name, so that it is the same modules on
-- every run and machine.
sampled :: Text -> Bool
sampled m = T.foldl' step 14695981039346656037 m `mod` 8 == (0 :: Word64)
  where
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211

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
