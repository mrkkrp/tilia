module Route.Match where

resolve :: Request -> Either String Handler
resolve request =
    checked $ case (methodOf request, pathOf request) of
        ( Get
            , ["orders", orderId]
            ) -> Right (showOrder orderId)
        (Post, ["orders"]) -> Right createOrder
        _ -> Left "no route"

prefix :: [Segment] -> Maybe Segment
prefix segments = case segments of
    [ first
        , second
        ] -> Just (first <> second)
    [only] -> Just only
    _ -> Nothing

leading :: Segment -> Segment -> Segment
leading this that = case [this, that] of
    ( first
        : _
        ) -> first
    [] -> root
