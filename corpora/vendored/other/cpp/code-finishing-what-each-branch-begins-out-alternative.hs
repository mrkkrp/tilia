{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

-- What follows a conditional finishes the construct each of its branches
-- begins, and stays after the conditional rather than going into each
-- branch.
module Session.Store where

import Control.Arrow ((>>>))
import Control.Monad (forM_)

-- Left-hand sides.
lookupKey :: Maybe (Int, Int) -> Int -> Int
#if MIN_VERSION_base(4,16,0)
lookupKey (Just (k, _)) d =
#else
lookupKey (Just (k, _)) _d =
#endif
  k + 1
lookupKey Nothing d = d

-- Alternatives of a case.
expiry :: Maybe (Int, Int) -> Int
expiry m = case m of
#if MIN_VERSION_base(4,16,0)
  Just (_, t) ->
#else
  Just (t, _) ->
#endif
    t * 60
  Nothing -> 0

-- Signatures.
#if MIN_VERSION_base(4,16,0)
store ::
  (Show k, Ord k) =>
#else
store ::
  (Show k) =>
#endif
  k -> String
store = show

-- Applications.
#if MIN_VERSION_base(4,16,0)
render k =
  pad
    4
#else
render k =
  pad
    4
    ' '
#endif
    (show k)

-- Blocks.
dump :: [Int] -> IO ()
dump ks = do
#if MIN_VERSION_base(4,16,0)
  forM_ ks $ \k -> do
#else
  forM_ (reverse ks) $ \k -> do
#endif
    print k
    print (k * 2)

-- The alternatives of a lambda case.
classify :: Int -> Int
#if MIN_VERSION_base(4,16,0)
classify =
  negate >>> abs >>> \case
#else
classify =
  abs >>> \case
#endif
    0 -> 1
    n -> n

-- The qualifiers of a list comprehension.
names :: [(Int, String)] -> [String]
#if MIN_VERSION_base(4,16,0)
names xs =
  [ show n ++ s
#else
names xs =
  [ s
#endif
  | (n, s) <- xs
  ]
