{-# OPTIONS_GHC  -Wall    -Wno-orphans #-}
{-# OPTIONS_GHC "-DGREETING=\"hello,  world\""   -Wno-unused-imports #-}

-- The flags of an options pragma are one space apart, but for the spaces
-- inside a string literal, which GHC reads as part of one flag.
module Greeting where
