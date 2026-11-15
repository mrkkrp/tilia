{-# LANGUAGE CPP #-}

module Database.Esqueleto.Record (RecordInfo (..)) where

data RecordInfo = RecordInfo
  { name :: Name
    -- ^ The original record's name.
#if MIN_VERSION_template_haskell(2,21,0)
  , typeVarBinders :: [TyVarBndr BndrVis]
#elif MIN_VERSION_template_haskell(2,17,0)
  , typeVarBinders :: [TyVarBndr ()]
#else
  , typeVarBinders :: [TyVarBndr]
#endif
    -- ^ The original record's type-variable-binders.
  , kind :: Maybe Kind
    -- ^ The original record's kind, I think.
  }
