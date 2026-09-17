{-# LANGUAGE LambdaCase #-}

-- | Running a program and reading its output.
module Tilia.Process
  ( readProgramOutput,
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import System.Exit (ExitCode (..))
import System.IO (Handle, hClose, hSetBinaryMode)
import System.Process
  ( StdStream (CreatePipe),
    proc,
    std_err,
    std_in,
    std_out,
    waitForProcess,
    withCreateProcess,
  )
import Tilia.Newline (NewlineStyle (Lf), setNewlineStyle)
import Tilia.Utils (quietly)

-- | Run a program and read what it printed on standard output.
--
-- 'Nothing' where it could not be run at all or did not succeed.
--
-- Line endings come back as newlines however the program wrote them.
readProgramOutput :: FilePath -> [String] -> IO (Maybe Text)
readProgramOutput program args = quietly Nothing $
  withCreateProcess spec $ \toChild fromChild childErrors running -> do
    traverse_ hClose toChild
    waitForErrors <- forked (drain childErrors)
    out <- drain fromChild
    _ <- waitForErrors
    waitForProcess running >>= \case
      ExitSuccess -> pure (Just (setNewlineStyle Lf (T.decodeUtf8Lenient out)))
      _ -> pure Nothing
  where
    spec =
      (proc program args)
        { std_in = CreatePipe,
          std_out = CreatePipe,
          std_err = CreatePipe
        }

-- | Read a pipe to its end.
drain :: Maybe Handle -> IO BS.ByteString
drain = \case
  Nothing -> pure BS.empty
  Just h -> quietly BS.empty (hSetBinaryMode h True >> BS.hGetContents h)

-- | Start an action now and hand back the waiting callback for it.
forked :: IO a -> IO (IO a)
forked action = do
  done <- newEmptyMVar
  _ <- forkIO (action >>= putMVar done)
  pure (takeMVar done)
