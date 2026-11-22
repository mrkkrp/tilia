retry x =
    if x
        then -- once more
            do attempt x
        else -- for good
            do giveUp x

classify n
    | n > 0 = -- the usual case
        do { report n; pure n }
    | otherwise = -- the rest
        case n of { 0 -> pure 0; _ -> pure (negate n) }

pick x = case x of
    Just n
        | n > 0 -> -- positive
            \k -> k n
    _ -> id

fallBack nested =
    if nested
        then pure ()
        else -- nested mode
            do error $ "fallBack: unexpected nested mode: " ++ show nested ++ " here"
