{-|
Module      : Headroom.Global
Description : Global data types and programmatic configuration
Copyright   : (c) 2019-2020 Vaclav Svejcar
License     : BSD-3
Maintainer  : vaclav.svejcar@gmail.com
Stability   : experimental
Portability : POSIX

Contains data types and configuration that needs to be expressed as compile
time code.
-}
{-# LANGUAGE NoImplicitPrelude #-}
module Headroom.Global
  ( TemplateType
  )
where

import           Headroom.Template.Mustache     ( Mustache )

-- | Type of the template format.
type TemplateType = Mustache
