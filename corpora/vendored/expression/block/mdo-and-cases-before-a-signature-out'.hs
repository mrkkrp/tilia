{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

module Signal.Loop where

feedback = mdo { out <- delay 0 next; next <- step out; pure out } :: Wire Int

joined = start <> mdo { rest <- follow rest; pure rest } :: Wire Int

pick = \cases { True x _ -> x; False _ y -> y } :: Bool -> Int -> Int -> Int

settle =
    mdo
        level <- drain flow
        flow <- pump level
        pure level
    :: Wire Int
