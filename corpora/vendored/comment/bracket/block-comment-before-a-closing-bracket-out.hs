module Terminal.Keys where

import Data.ByteString.Char8 ({- IsString -})
import Data.List hiding (sort {- the local one -})

recordKey :: Key -> IO (Bool {- already bound -}, Int)
recordKey = undefined

modifiers = (shift {- left or right -}, control {- either -})

arrows = [up, down, left, right {- and no others -}]

pressed = bind (key {- as reported -}) action

pending = [{- none yet -}]
