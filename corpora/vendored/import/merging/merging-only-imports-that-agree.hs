{-# LANGUAGE PackageImports #-}
{-# LANGUAGE Trustworthy #-}

module Billing.Report where

import qualified Data.Text as Text (pack)
import Data.Text (Text)
import "text" Data.Text (unpack)
import qualified Data.Text as T (pack)
import Data.Text hiding (map)
import safe Data.Text (strip)
import qualified Data.Text (toUpper)
import qualified Data.Text as T (unpack)
import "text" Data.Text (words)
import Data.Text (lines)

title :: Text -> Text
title = T.pack . T.unpack
