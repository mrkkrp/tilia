{-# LANGUAGE OverloadedStrings #-}

-- | Interface files read directly, against what @ghc --show-iface@ makes of
-- them, for the modules of the packages this project is built against.
--
-- Every module is decoded, but only those that offer fixities or hold a
-- key the table passed over, and a fixed eighth of the rest, are checked
-- against @ghc@, which costs a process per module and was most of the
-- suite's run time.
module Tilia.Fixity.HiFileSpec (spec) where

import Control.Applicative ((<|>))
import Control.Exception (evaluate)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Foldable (for_)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Test.Hspec
import Tilia.Fixity (Fixities, Namespace (..), OpName (..))
import Tilia.Fixity.HiFile
  ( HiExport (..),
    HiFile (..),
    HiName (..),
    decodeHiFile,
    primopFixities,
  )
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
  declaredIn <- runIO (Map.fromList . concat <$> traverse declarations modules)
  let declared m = Map.lookup m declaredIn <|> Map.lookup m primopFixities
  describe "interface files read directly" $ do
    it "are there to read" $
      length modules `shouldSatisfy` (>= 500)

    it "decode, every one of them" $ do
      failures <- concat <$> inParallel undecodable modules
      failures `shouldBe` []

    for_ (concatMap testsFor dependencies) $ \(label, chunk) ->
      it ("agree with ghc --show-iface on " <> label) $ do
        found <- concat <$> inParallel (disagreeing declared) chunk
        found `shouldBe` []
  where
    declarations (m, path) = do
      bytes <- BS.readFile path
      pure
        [ (m, Map.fromList [((namespace, op), fixity) | (namespace, op, fixity) <- hiFixities hi])
        | Right hi <- [decodeHiFile bytes]
        ]
    undecodable (m, path) = do
      bytes <- BS.readFile path
      evaluate [(m, why) | Left why <- [decodeHiFile bytes]]
    -- Forced before it is returned, so that the output of ghc, which is
    -- hundreds of megabytes across a run, is not kept alive until the end.
    disagreeing declared (m, path) = do
      decoded <- either (const Nothing) Just . decodeHiFile <$> BS.readFile path
      case decoded of
        Just hi
          | Right d <- fromHiFile m hi,
            not (Set.null (offered declared m d)) || passesOver hi || sampled m -> do
              shown <-
                maybe Nothing (parseInterface m)
                  <$> readProgramOutput "ghc" ["--show-iface", path]
              let found = [(m, x) | Just s <- [shown], x <- disagreements declared m d s]
              found <$ evaluate (length found)
        _ -> pure []

-- | Whether an interface holds a key the table for its series passed over.
--
-- A key the table should have listed would be passed over too, and the
-- module could then look as if it offered nothing.
passesOver :: HiFile -> Bool
passesOver = any (elem Unneeded . names) . hiExports
  where
    names (Avail n) = [n]
    names (AvailTC p ns) = p : ns

-- | Whether a module is in the fixed eighth of the rest, which are checked
-- against @ghc@ all the same.
--
-- Chosen by an FNV-1a hash of the name, so that it is the same modules on
-- every run and machine.
sampled :: Text -> Bool
sampled m = T.foldl' step 14695981039346656037 m `mod` 8 == (0 :: Word64)
  where
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211

-- | Where a direct reading and the text disagree about what fixities depend
-- on, beyond what the text cannot tell.
--
-- A fixity the text puts among terms may be among types all the same: the
-- text leaves out the declarations of the types GHC wires in.
disagreements ::
  -- | The fixities each module declares, where they can be read.
  (Text -> Maybe Fixities) ->
  Text ->
  Interface ->
  Interface ->
  [Text]
disagreements declared m direct shown =
  [ "fixities"
  | not (inTypes (interfaceDeclares shown) `Map.isSubmapOf` interfaceDeclares direct)
      || byOperator (interfaceDeclares direct) /= byOperator (interfaceDeclares shown)
  ]
    <> ["offered fixities" | offered declared m direct /= offered declared m shown]
    <> ["what types carry" | carrying direct /= carrying shown]
  where
    inTypes = Map.filterWithKey (\(namespace, _) _ -> namespace == InTypes)
    carrying i =
      Map.filter (not . Set.null) $
        Map.map (Set.filter (`Set.member` withFixities)) (interfaceChildren i)
    withFixities = Set.map (OpName . fst) (offered declared m direct)

-- | Every fixity a module offers, its own and those of what it reexports,
-- by operator.
offered :: (Text -> Maybe Fixities) -> Text -> Interface -> Set (Text, String)
offered declared m i =
  byOperator (interfaceDeclares i)
    <> Set.unions
      [ byOperator (Map.filterWithKey (\(_, o) _ -> o == op) fixities)
      | (from, op) <- interfaceReexports i,
        from /= m,
        Just fixities <- [declared from]
      ]

-- | Fixities by the operator alone, namespace aside.
byOperator :: Fixities -> Set (Text, String)
byOperator = Set.fromList . fmap (\((_, OpName op), fixity) -> (op, show fixity)) . Map.toList
