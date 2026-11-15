{-# LANGUAGE CPP #-}
module Terminal.Markup where

import Data.Text (Text)
import Control.Arrow (second)
import Control.Monad (when)
import Text.Blaze (Markup, toMarkup)
#define Html Markup
#define toHtml toMarkup
import Data.Maybe (fromMaybe)
import Control.Arrow (first)

label :: Text -> Html
label  =  toHtml
