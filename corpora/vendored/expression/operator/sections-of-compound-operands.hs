module Price.Adjust where

withTax :: [Double] -> [Double]
withTax = map (rate * base +) . map (* (1 + rate))

tagged :: [String] -> [String]
tagged = map (++ [marker]) . map ((prefix, suffix) <>)
