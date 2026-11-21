{-# LANGUAGE CPP #-}

module Game.Strategy where

simple :: Strategy
simple = Strategy
#ifdef DEBUG
           "simple"
#endif
           $ \board -> playSafe board

main :: IO ()
main = do
  result <- withStrategy
#ifdef DEBUG
              trace
#endif
              $ \board -> play board
  print result

pick :: Maybe Board -> Strategy
pick b = case b of
  Just board -> Strategy
#ifdef DEBUG
                  "picked"
#endif
                  $ do
    playSafe board
  Nothing -> simple

rollout :: Board -> IO Strategy
rollout board = do
  let suggested = withMoves $ Strategy
#ifdef DEBUG
                                "suggested"
#endif
                                $ \moves -> pick board moves
  pure suggested
