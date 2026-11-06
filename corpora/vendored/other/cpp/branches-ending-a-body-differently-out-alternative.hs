{-# LANGUAGE CPP #-}

module Console.Encoding where

unicodeSupported :: Handle -> IO Bool
unicodeSupported h = do
#ifndef __MHS__
  (== Just "UTF-8") . fmap show <$> hGetEncoding h
#else
  return True

canonicalize :: FilePath -> IO FilePath
canonicalize = return
#endif
