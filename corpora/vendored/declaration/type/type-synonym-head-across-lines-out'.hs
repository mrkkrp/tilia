module Http.Route where

type Handler
    request
    response =
    request -> IO response

type Middleware
    request
    response =
    Handler request response -> Handler request response

type ( failure
        `Or` success
     )
    context =
    Either (failure, context) (success, context)
