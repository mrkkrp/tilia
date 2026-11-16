module Courier.Route where

deliver :: Parcel -> IO ()
deliver parcel = do
    ( if fragile parcel
        then carefully
        else quickly
        )
        dispatch
        parcel
    (if urgent parcel then express else standard) parcel
    ( notify
        (recipient parcel)
        )
    (track parcel)
