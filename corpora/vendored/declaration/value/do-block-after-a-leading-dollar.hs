textRoundtrip :: IO ()
textRoundtrip =
  fmap id
    $ withConnection
    $ do
      encode
      decode

roundtrip :: IO ()
roundtrip =
  withConnection
    $ do
      encode
      decode

withRemark :: IO ()
withRemark =
  withConnection    -- a fresh connection
    $ do
      encode
      decode
