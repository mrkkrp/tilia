{-# LANGUAGE CPP #-}

module M where

import Data.Maybe (catMaybes)
import Language.Haskell.TH.Syntax
#if MIN_VERSION_template_haskell(2,19,0)
  hiding (makeRelativeToProject)
#endif
import System.Directory (doesFileExist)
import Yesod.Core
