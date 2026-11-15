{-# LANGUAGE RecursiveDo #-}

module Signal.Fallback where

import Control.Applicative ((<|>))

latch input =
    mdo held <- hold input out; out <- gate held; pure out
    <|> pure input

loop input =
    mdo out <- feed input out; pure out
    <|> idle

settle = do
    level <-
        mdo a <- probe b; b <- probe a; pure a
        <|> pure 0
    pure level
