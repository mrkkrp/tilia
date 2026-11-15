{-# LANGUAGE QuasiQuotes #-}

exceptions :: [String] -> String
exceptions es = [i|
    Encountered one or more exceptions.
  |] <> concat es

unexpected :: Int -> Maybe String
unexpected sp = Just (render sp [i|
    Something went wrong.
  |] Nothing)

checked :: IO ()
checked = withMessage [i|
    Checking.
  |] $ do
  check
  report

check :: Int
check = 1
{-# ANN check (Prim [Verilog, VHDL] [i|
  BlackBox:
    name: check
  |]) #-}
