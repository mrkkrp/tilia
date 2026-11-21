{-# LANGUAGE CPP #-}

module Game.Strategy where

simple :: Strategy
simple =
#ifdef DEBUG
    Strategy
        "simple"
        $ \board -> playSafe board
#else
    Strategy $
        \board -> playSafe board
#endif

main :: IO ()
main = do
    result <-
#ifdef DEBUG
        withStrategy
            trace
            $ \board -> play board
#else
        withStrategy $
            \board -> play board
#endif
    print result

pick :: Maybe Board -> Strategy
pick b = case b of
    Just board ->
#ifdef DEBUG
        Strategy
            "picked"
            $ do
                playSafe board
#else
        Strategy $ do
            playSafe board
#endif
    Nothing -> simple

rollout :: Board -> IO Strategy
rollout board = do
    let suggested =
            withMoves
#ifdef DEBUG
                $ Strategy
                    "suggested"
                $ \moves -> pick board moves
#else
                $
                    Strategy $
                        \moves -> pick board moves
#endif
    pure suggested
