{-# LANGUAGE TemplateHaskell #-}

module Schema.Derive where

handlerType :: Q Type
handlerType = [t|Request -> IO Response|]

pairType :: Q Type
pairType = [t|(Key, [Value])|]
