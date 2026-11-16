{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Library where

import Servant.API

type LibraryApi =
  "books"
    :> ( Get '[JSON] [Book]
           :<|> Capture "isbn" Isbn :> Get '[JSON] Book
       )
    :<|> "loans"
      :> ( "open" :> Get '[JSON] [Loan]
             :<|> "closed" :> Get '[JSON] [Loan]
         )
