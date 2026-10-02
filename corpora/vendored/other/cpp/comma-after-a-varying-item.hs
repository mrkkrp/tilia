{-# LANGUAGE CPP #-}

module Records.Info where

data RecordInfo = RecordInfo
  { name :: Name
  , constraints :: Cxt
#if MIN_VERSION_template_haskell(2,21,0)
  , typeVarBinders :: [TyVarBndr BndrVis]
#elif MIN_VERSION_template_haskell(2,17,0)
  , typeVarBinders :: [TyVarBndr ()]
#else
  , typeVarBinders :: [TyVarBndr]
#endif
  , kind :: Maybe Kind
  }
