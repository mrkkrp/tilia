-- An import hiding an operator brings no fixity for it, while one hiding
-- the members of a class still brings that of <>, so the ++ this module
-- defines without a fixity is infixl 9 and binds tighter than <>.
module Path.Segments where

import Prelude hiding (Foldable (..), (++))

newtype Path = Path [String]

instance Semigroup Path where
  Path a <> Path b = Path (a <> b)

(++) :: Path -> String -> Path
Path segments ++ segment = Path (segments <> [segment])

null :: Path -> Bool
null (Path segments) = segments == []

logs :: Path -> Path -> Path
logs root home = root
  ++ "var"
  <> home
  ++ "log"
