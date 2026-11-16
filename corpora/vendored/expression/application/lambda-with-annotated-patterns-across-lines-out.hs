module Codec.Laws where

roundTrips :: Spec
roundTrips =
  it "decodes what it encodes" $
    \(codec :: Codec (Maybe Int))
     (value :: Maybe Int) ->
        decode codec (encode codec value) == Right value
