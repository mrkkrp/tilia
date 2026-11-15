{-# LANGUAGE CPP #-}

module Config.Load where

#if MIN_VERSION_base(4,16,0)
load (Just path) verbose = -- with a path
#else
load path verbose = -- without one
#endif
  -- before the body
  readConfig path verbose -- after the body
-- after the declaration

retry r =
#if MIN_VERSION_base(4,16,0)
  backoff r -- end of the first branch
#else
  backoff r 2
#endif
    -- before the argument
    (limit r) -- trailing
    -- under the argument
