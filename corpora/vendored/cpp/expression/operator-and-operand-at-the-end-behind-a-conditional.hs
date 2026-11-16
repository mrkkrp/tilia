{-# LANGUAGE CPP #-}

module Kitchen.Recipes (readRecipe) where

readRecipe :: FilePath -> IO Text
readRecipe path =
  T.readFile path
#if defined(BUNDLED)
  `catch` \(e :: IOException) ->
    maybe (throwIO e)
          (pure . decodeUtf8)
          (lookup path bundled)
#endif
