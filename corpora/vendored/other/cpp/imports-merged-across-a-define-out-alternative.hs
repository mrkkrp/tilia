{-# LANGUAGE CPP #-}

module Terminal.Markup where

import Control.Arrow (first, second)
import Control.Monad (when)
#define Html Markup
#define toHtml toMarkup
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Text.Blaze (Markup, toMarkup)

label :: Text -> Html
label = toHtml
