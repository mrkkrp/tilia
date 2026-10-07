{-# LANGUAGE CPP #-}

module Lens.Classy where

makeClassy :: Name -> Q [Dec]
makeClassy name = do
  methods <- classMethods name
#if MIN_VERSION_template_haskell(2,21,0)
  classD (cxt []) name [] [] [] methods $
#else
  classD (cxt []) name [] [] methods $
#endif
    sigD (mkName "lensOf") (conT name)
      : fmap pure methods
