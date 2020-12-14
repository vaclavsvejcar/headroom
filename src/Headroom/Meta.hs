{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

{-|
Module      : Headroom.Meta
Description : Application metadata (name, vendor, etc.)
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3-Clause
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Module providing application metadata, such as application name, vendor,
version, etc.
-}

module Headroom.Meta
  ( TemplateType
  , buildVersion
  , configBreakingChanges
  , configFileName
  , productDesc
  , productInfo
  , productName
  , webDoc
  , webDocMigration
  , webRepo
  )
where

import           Data.Version                        ( showVersion )
import           Headroom.Meta.Version               ( Version(..)
                                                     , parseVersion
                                                     , printVersion
                                                     , pvp
                                                     )
import           Headroom.TemplateSupport.Mustache   ( Mustache )
import           Paths_headroom                      ( version )
import           RIO
import           RIO.Partial                         ( fromJust )
import qualified RIO.Text                           as T


-- | Type of the template format used for license headers.
type TemplateType = Mustache


-- | Application version, as specified in @headroom.cabal@ file.
buildVersion :: Version
buildVersion = fromJust . parseVersion . T.pack . showVersion $ version


-- | List of versions that made breaking changes into YAML configuration and
-- require some migration steps to be performed by end-user.
configBreakingChanges :: [Version]
configBreakingChanges = [[pvp|0.4.0.0|]]


-- | Name of the YAML configuration file.
configFileName :: IsString a => a
configFileName = ".headroom.yaml"


-- | Full product description.
productDesc :: Text
productDesc = "manage your source code license headers"


-- | Product info.
productInfo :: Text
productInfo =
  mconcat [productName, ", v", printVersion buildVersion, " :: ", webRepo]


-- | Product name.
productName :: Text
productName = "headroom"


-- | Product documentation website for given version.
webDoc :: Version -> Text
webDoc v = "http://doc.norcane.com/headroom/v" <> printVersion v


-- | Product migration guide for given version.
webDocMigration :: Version -> Text
webDocMigration v = webDoc v <> "/migration-guide"


-- | Product source code repository.
webRepo :: Text
webRepo = "https://github.com/vaclavsvejcar/headroom"
