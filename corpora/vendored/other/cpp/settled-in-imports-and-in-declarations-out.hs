{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
#endif
#ifdef USE_ST
{-# LANGUAGE RankNTypes #-}
#endif

module Graph where

import Data.List (sort)
#ifdef __GLASGOW_HASKELL__
import Language.Haskell.TH.Syntax (Lift (..))
#endif

data SCC v = AcyclicSCC v

#ifdef __GLASGOW_HASKELL__
deriving instance (Show v) => Show (SCC v)

#if MIN_VERSION_template_haskell(2,15,0)
deriving instance (Lift v) => Lift (SCC v)
#else
instance (Lift v) => Lift (SCC v) where
  lift (AcyclicSCC v) = [|AcyclicSCC v|]
#endif
#endif

#if USE_ST
run :: (forall s. s -> s) -> Int
run f = f 1
#endif

f :: Int
f = 1
