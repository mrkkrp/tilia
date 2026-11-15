{-# LANGUAGE CPP #-}

module Database.Esqueleto.Record (RecordInfo (..)) where

data RecordInfo = RecordInfo
  { -- | The original record's name.
    name :: Name,
#if MIN_VERSION_template_haskell(2,21,0)
    -- | The original record's type-variable-binders.
    typeVarBinders :: [TyVarBndr BndrVis],
#elif MIN_VERSION_template_haskell(2,17,0)
    -- | The original record's type-variable-binders.
    typeVarBinders :: [TyVarBndr ()],
#else
    -- | The original record's type-variable-binders.
    typeVarBinders :: [TyVarBndr],
#endif
    -- | The original record's kind, I think.
    kind :: Maybe Kind
  }
