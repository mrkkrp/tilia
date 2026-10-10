{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | What the benchmarks of fixity resolution measure: reading every module
-- of a few packages of the Hackage corpus out of their tarballs, the same
-- out of a cache that an earlier reading filled, and decoding every
-- interface of a few packages the compiler ships with.
module Tilia.Bench.Fixity (withFixityBenchmarks) where

import Control.DeepSeq (force)
import Control.Exception (bracket_, evaluate)
import Control.Monad (filterM, join, unless)
import Crypto.Hash.SHA256 qualified as SHA256
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Choice (pattern Do, pattern Don't)
import Data.Either (rights)
import Data.Foldable (for_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Traversable (for)
import System.Directory
  ( createDirectoryIfMissing,
    createFileLink,
    doesFileExist,
    removePathForcibly,
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((<.>), (</>))
import System.Process (getCurrentPid)
import Text.Printf (printf)
import Tilia.Bench.Cases (Benchmark (..), Stage (..))
import Tilia.Corpus (Corpus (..), Source (..), corpusCache, hackagePackages)
import Tilia.Fixity.Cabal (packageModules)
import Tilia.Fixity.Cache (Cache, PlanToken (..), openCache)
import Tilia.Fixity.HiFile (decodeHiFile)
import Tilia.Fixity.Interface (fromHiFile)
import Tilia.Fixity.PackageDb
  ( Installed (..),
    InstalledPackage (..),
    readInstalledPackages,
  )
import Tilia.Fixity.Plan

-- | The packages of the Hackage corpus whose every module is read.
resolved :: [String]
resolved =
  [ "lens-5.3.6",
    "aeson-2.3.1.0",
    "megaparsec-9.8.1",
    "servant-0.20.3.0",
    "conduit-1.3.6.1",
    "vector-0.13.2.0",
    "text-2.1.4",
    "free-5.2",
    "optics-0.4.2.1",
    "semigroupoids-6.0.2",
    "pandoc-types-1.23.1.2",
    "mtl-2.3.2"
  ]

-- | The packages the compiler ships with whose every interface is decoded.
decoded :: [Text]
decoded =
  [ "base",
    "ghc-internal",
    "containers",
    "template-haskell",
    "bytestring",
    "text"
  ]

-- | The packages the corpus's packages depend on and it does not hold, at
-- the versions the compiler ships or versions those packages accept, which
-- the plan names so that the macros of their versions are defined.
dependencies :: [(Text, Text)]
dependencies =
  [ ("StateVar", "1.2.2"),
    ("array", "0.5.8.0"),
    ("base", "4.20.2.0"),
    ("binary", "0.8.9.3"),
    ("bytestring", "0.12.2.0"),
    ("containers", "0.7"),
    ("deepseq", "1.5.0.0"),
    ("directory", "1.3.8.5"),
    ("distributive", "0.6.3"),
    ("filepath", "1.5.4.0"),
    ("ghc-prim", "0.12.0"),
    ("http-media", "0.8.1.1"),
    ("indexed-traversable", "0.1.5"),
    ("mono-traversable", "1.0.21.0"),
    ("os-string", "2.0.7"),
    ("primitive", "0.9.1.0"),
    ("process", "1.6.26.1"),
    ("tagged", "0.8.11"),
    ("tasty-inspection-testing", "0.2.1"),
    ("template-haskell", "2.22.0.0"),
    ("unix", "2.8.7.0")
  ]

-- | Set the benchmarks of fixity resolution up in a directory of their own,
-- run what is given on them and on a digest of the interfaces they decode,
-- which what decoding them allocates depends on, and remove the directory.
--
-- The directory's path is as long on every machine, since what reading the
-- cache and the tarballs allocates depends on the length of their paths.
-- Setting them up leaves pinned data in the heap, which moves where the
-- collections fall in the benchmarks that run after it, so this goes after
-- the benchmarks of formatting.
withFixityBenchmarks :: (([Benchmark], Text) -> IO a) -> IO (Either Text a)
withFixityBenchmarks use = do
  pid <- getCurrentPid
  let dir = printf "/tmp/tilia-bench-%010d" (fromIntegral pid :: Int)
  bracket_
    (createDirectoryIfMissing True (dir </> "tarballs"))
    (removePathForcibly dir)
    (fixityBenchmarks dir >>= traverse use)

-- | The benchmarks of fixity resolution, set up in the directory given, and
-- a digest of the interfaces they decode.
fixityBenchmarks :: FilePath -> IO (Either Text ([Benchmark], Text))
fixityBenchmarks dir = do
  corpus <- (</> corpusName hackagePackages) <$> corpusCache
  let releases = case corpusSource hackagePackages of
        HackageReleases names -> names
        _ -> []
      tarballOf release = dir </> "tarballs" </> release <.> "tar.gz"
      tarballs = [(planned release, tarballOf release) | release <- releases]
      plan =
        BuildPlan
          { bpCompiler = "ghc-9.10.3",
            bpPackages =
              [PlanPackage n v PreExisting [] | (n, v) <- dependencies]
                <> fmap fst tarballs
          }
      fetched release = corpus </> release <.> "tar.gz"
  missing <- filterM (fmap not . doesFileExist . fetched) releases
  case missing of
    release : _ ->
      pure (Left ("No tarball at " <> T.pack (fetched release) <> "."))
    [] -> do
      for_ releases $ \release ->
        createFileLink (fetched release) (tarballOf release)
      none <- openCache (Don't #useCache) (PlanToken "")
      cache <- cacheUnder (dir </> "cache")
      interfaces <- interfacesOf decoded
      let reading kept modules = do
            resolver <- newResolverWith [FromSource] kept [] tarballs plan
            pure $ do
              answers <- traverse (askModule resolver) modules
              length <$> evaluate (force answers)
      packages <- for resolved $ \release -> do
        modules <- fromMaybe [] <$> packageModules (tarballOf release)
        filled <- newIORef False
        let recalling = do
              done <- readIORef filled
              unless done $ do
                _ <- join (reading cache modules)
                writeIORef filled True
              reading cache modules
        pure
          [ Benchmark Resolve (T.pack release) (reading none modules),
            Benchmark Recall (T.pack release) recalling
          ]
      pure $
        Right
          ( concat packages
              <> [ Benchmark Decode name (decoding files)
                 | (name, files) <- interfaces
                 ],
            digestOf interfaces
          )
  where
    planned release =
      let (name, version) = T.breakOnEnd "-" (T.pack release)
       in PlanPackage (T.dropEnd 1 name) version PreExisting []
    decoding files = do
      fresh <- evaluate (force [(m, BS.copy bytes) | (m, bytes) <- files])
      pure $
        length . rights
          <$> evaluate
            (force [fromHiFile m =<< decodeHiFile bytes | (m, bytes) <- fresh])

-- | A cache of fixities in a directory.
cacheUnder :: FilePath -> IO Cache
cacheUnder dir = do
  before <- lookupEnv "XDG_CACHE_HOME"
  setEnv "XDG_CACHE_HOME" dir
  cache <- openCache (Do #useCache) (PlanToken "benchmarks")
  maybe (unsetEnv "XDG_CACHE_HOME") (setEnv "XDG_CACHE_HOME") before
  pure cache

-- | The interfaces of every module of the installed packages named, each
-- package by its name and version.
interfacesOf :: [Text] -> IO [(Text, [(Text, BS.ByteString)])]
interfacesOf names = do
  installed <- installedPackages <$> readInstalledPackages
  for [p | n <- names, p <- installed, ipName p == n] $ \p -> do
    files <- for (ipModules p) $ \m -> do
      let file = T.unpack (T.replace "." "/" m) <.> "hi"
      found <- filterM doesFileExist [dir </> file | dir <- ipImportDirs p]
      for (listToMaybe found) (fmap ((,) m) . BS.readFile)
    pure (ipName p <> "-" <> ipVersion p, catMaybes files)

-- | A digest of the names and sizes of interfaces, which what decoding them
-- costs depends on: two builds of a compiler can differ in a fingerprint
-- written in one, which costs nothing more to decode.
digestOf :: [(Text, [(Text, BS.ByteString)])] -> Text
digestOf interfaces =
  T.take 16 . T.decodeUtf8 . B16.encode . SHA256.hash . T.encodeUtf8 $
    T.unlines
      [ T.unwords [name, m, T.pack (show (BS.length bytes))]
      | (name, files) <- interfaces,
        (m, bytes) <- files
      ]
