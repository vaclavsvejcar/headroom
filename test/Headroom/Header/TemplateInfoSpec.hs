{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module Headroom.Header.TemplateInfoSpec
  ( spec
  )
where

import           Headroom.Configuration              ( makeHeadersConfig
                                                     , parseConfiguration
                                                     )
import           Headroom.Configuration.Types        ( Configuration(..)
                                                     , HeaderConfig
                                                     , HeaderSyntax(..)
                                                     , LicenseType(..)
                                                     )
import           Headroom.Data.Lens                  ( suffixLensesFor )
import           Headroom.Data.Regex                 ( re )
import           Headroom.Embedded                   ( defaultConfig
                                                     , licenseTemplate
                                                     )
import           Headroom.FileType.Types             ( FileType(..) )
import           Headroom.Header.TemplateInfo
import           Headroom.Header.Types               ( TemplateInfo )
import           Headroom.Template                   ( Template(..) )
import           RIO
import           Test.Hspec


suffixLensesFor ["tiConfig"] ''TemplateInfo
suffixLensesFor ["hcHeaderSyntax"] ''HeaderConfig

spec :: Spec
spec = do

  describe "mkTemplateInfo" $ do
    it "creates TemplateInfo from given data" $ do
      template       <- parseTemplate Nothing (licenseTemplate BSD3 Java)
      defaultConfig' <- parseConfiguration defaultConfig
      config         <- makeHeadersConfig (cLicenseHeaders defaultConfig')
      let comment  = BlockComment [re|^\/\*|] [re|\*\/$|] (Just " *")
          actual   = mkTemplateInfo config Java template
          expected = actual & tiConfigL . hcHeaderSyntaxL .~ comment
      mkTemplateInfo config Java template `shouldBe` expected
