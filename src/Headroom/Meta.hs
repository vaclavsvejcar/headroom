{-|
Module      : Headroom.Meta
Description : Application metadata (name, vendor, etc.)
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing application metadata, such as application name, vendor,
version, etc.
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Headroom.Meta
  ( TemplateType
  , buildVersion
  , productDesc
  , productInfo
  , productName
  , website
  )
where

import           Data.Version                   ( showVersion )
import           Headroom.Template.Mustache     ( Mustache )
import           Paths_headroom                 ( version )
import           RIO
import qualified RIO.Text                      as T



-- | Type of the template format used for license headers.
type TemplateType = Mustache


-- | Application version, as specified in @headroom.cabal@ file.
buildVersion :: Text
buildVersion = T.pack . showVersion $ version


-- | Full product description.
productDesc :: Text
productDesc = "manage your source code license headers"


-- | Product info.
productInfo :: Text
productInfo = mconcat [productName, ", v", buildVersion, " :: ", website]


-- | Product name.
productName :: Text
productName = "headroom"


-- | Homepage website of the product.
website :: Text
website = "https://github.com/vaclavsvejcar/headroom"
