{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Text.IO qualified as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_tilia (version)
import System.Exit (ExitCode (..), exitWith)
import System.IO (stderr)
import Tilia.Format
  ( describeFormatError,
    formatErrorExitCode,
    formatFile,
  )
import Tilia.Package (newPackageReader)

main :: IO ()
main = do
  Opts {..} <- execParser optsParserInfo
  askPackage <- newPackageReader
  result <- formatFile askPackage optInputFile
  case result of
    Left e -> do
      T.hPutStrLn stderr ("tilia: " <> describeFormatError e)
      exitWith (ExitFailure (formatErrorExitCode e))
    Right output -> T.putStr output

----------------------------------------------------------------------------
-- Command line options

-- | The options a run was given.
newtype Opts = Opts
  { -- | File to format.
    optInputFile :: FilePath
  }

optsParserInfo :: ParserInfo Opts
optsParserInfo =
  info (helper <*> versionOption <*> optsParser) . mconcat $
    [ fullDesc,
      progDesc "Format Haskell source code",
      header "tilia - a formatter for Haskell source code"
    ]
  where
    versionOption =
      infoOption
        ("tilia " ++ showVersion version)
        (long "version" <> short 'v' <> help "Print version of the program")

optsParser :: Parser Opts
optsParser =
  Opts
    <$> (strArgument . mconcat)
      [ metavar "FILE",
        help "Haskell source file to format"
      ]
