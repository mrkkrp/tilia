{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

-- A let that ends a guard goes between braces, which end its bindings
-- before the = or ->, where without them only a parse error ends them and
-- an = after a signature continues the signature. A let that another
-- qualifier follows needs none, as the comma ends it.
module Core.Utils where

mergeAlts alts
    | let { merged = foldr combine [] alts
          ; merged :: [Alt]
          } =
        merged

simplArg env arg | let { env' = zapSubst env } = simplExpr env' arg

rebuildCase scrut alts = case alts of
    [alt] | let { (con, rhs) = splitAlt alt } -> rebuild con rhs
    _ -> fallback scrut

pickCase n = if | let { half = n `div` 2 } -> half | otherwise -> n

lookupFloat = \cases
    | let { found = Nothing } -> found

exprSize e
    | let { size = case e of
                Var _ -> 1
                App f a -> exprSize f + exprSize a
          } =
        size

isTrivial e
    | Just v <- isVar e
    , let ty = varType v
    , isTrivialType ty =
        True
    | otherwise = False
