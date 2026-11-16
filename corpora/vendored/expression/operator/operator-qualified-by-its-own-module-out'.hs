-- An operator qualified by the name of the module that declares it takes
-- the fixity declared here.
module Path.Join where

infixr 5 </>

(</>) :: String -> String -> String
a </> b = a <> "/" <> b

config :: String -> String
config name =
    "etc"
        Path.Join.</> name
            <> ".d"
