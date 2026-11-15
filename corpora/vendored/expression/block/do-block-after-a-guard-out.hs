greet :: Maybe String -> IO ()
greet name
  | Just n <- name,
    not (null n) = do
      putStrLn "Hello,"
      putStrLn n
  | otherwise = pure ()

describe :: Maybe Int -> IO ()
describe x = case x of
  Just n
    | n > 0 -> do
        print n
        print (n * 2)
  _ -> pure ()

classify :: Int -> String
classify n
  | n > 0 = case n of
      1 -> "one"
      _ -> "many"
  | otherwise = "none"

withRemark :: Int -> IO ()
withRemark n
  | n > 0 = do -- only positive ones
      print n
      print n
  | otherwise = pure ()

withNote :: Int -> IO ()
withNote n
  | n > 0 =
      -- only positive ones
      do
        print n
        print n
  | otherwise = pure ()

withTwoRemarks :: Int -> IO ()
withTwoRemarks n
  | n > 0 = -- only positive ones
      do -- twice
        print n
        print n
  | otherwise = pure ()
