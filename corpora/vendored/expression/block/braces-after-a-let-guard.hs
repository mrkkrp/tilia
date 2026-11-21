-- A guard that ends with a let, before a right-hand side whose braces open
-- on the line after its separator.
module Core.Lint where

lintAlt alt
  | let rhs = altRhs alt
  = do { ty <- lintExpr rhs
       ; pure ty }
  | otherwise
  = failWith alt
