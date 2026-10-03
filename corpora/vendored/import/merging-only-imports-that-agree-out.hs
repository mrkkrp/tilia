{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}

module Billing.Report where

import Data.Text (Text, lines)
import Data.Text hiding (map)
import qualified Data.Text (toUpper)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text as Text (pack)
import safe Data.Text (strip)
import "text" Data.Text (unpack, words)

title :: Text -> Text
title = T.pack . T.unpack
