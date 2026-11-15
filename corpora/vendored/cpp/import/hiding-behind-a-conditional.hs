{-# LANGUAGE CPP #-}

module M where

import Yesod.Core
import System.Directory (doesFileExist)
import Language.Haskell.TH.Syntax
#if MIN_VERSION_template_haskell(2,19,0)
    hiding (makeRelativeToProject)
#endif
import Data.Maybe (catMaybes)
