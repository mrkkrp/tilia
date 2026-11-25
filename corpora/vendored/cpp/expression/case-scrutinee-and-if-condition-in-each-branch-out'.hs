{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module ImportStylePlugin.Compat (explicitlyImported, spaced) where

-- | The names an import lists, unless it hides them.
explicitlyImported :: ImportDecl GhcPs -> Maybe [IE GhcPs]
explicitlyImported ImportDecl{..} =
    case
#if __GLASGOW_HASKELL__ >= 906
        ideclImportList of
#else
        ideclHiding of
#endif
        Just (Exactly, L _ names) -> Just (fmap unLoc names)
        _ -> Nothing

-- | An argument put a space away from what it is applied to.
spaced :: LHsExpr GhcPs -> LHsExpr GhcPs
spaced e@(L _ (HsApp _ _ (L lb _))) =
    if
#if MIN_VERSION_ghc(9,12,0)
        manchorOp lb == Just (SameLine 0)
#else
        manchorOp (ann lb) == Just (SameLine 0)
#endif
        then setEntryDP e (SameLine 1)
        else e
spaced e = e
