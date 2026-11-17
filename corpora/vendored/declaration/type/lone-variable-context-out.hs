{-# LANGUAGE RequiredTypeArguments #-}

variable :: r => Int
variable = undefined

applied :: Show a => a -> a
applied = id

pair :: (r, s) => Int
pair = undefined

variableAndApplied :: (r, Show a) => a
variableAndApplied = undefined

nothing :: () => Int
nothing = undefined

quotedVariable = describe (r => Int)

quotedApplied = describe (Show a => a)

quotedPair = describe ((r, s) => Int)
