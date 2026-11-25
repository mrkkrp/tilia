{-# LANGUAGE CPP #-}

module GHC.SourceGen.Decl (infixCon, trySharedLock, forced) where

-- | Declares a Haskell-98-style constructor with an infix name.
infixCon :: Field -> OccNameStr -> Field -> ConDecl'
infixCon f name f' =
    renderCon98Decl name
#if MIN_VERSION_ghc(9,14,0)
        $ InfixCon (renderField f) (renderField f')
#else
        $ InfixCon
            (hsUnrestricted $ renderField f)
            (hsUnrestricted $ renderField f')
#endif

-- | Takes a shared lock on a file, unless another process holds it.
trySharedLock :: FilePath -> Annex (Maybe LockHandle)
trySharedLock lockfile =
    debugLocks
#ifndef mingw32_HOST_OS
        $ tryLockShared Nothing lockfile
#else
        $ liftIO
        $ lockShared lockfile
#endif

-- | Evaluates a value before it returns another.
forced :: a -> b -> b
forced x y =
    x
#if MIN_VERSION_base(4,20,0)
        `seq` y
#else
        `seq` id y
#endif
