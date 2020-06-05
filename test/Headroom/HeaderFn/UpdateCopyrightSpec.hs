{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Headroom.HeaderFn.UpdateCopyrightSpec
  ( spec
  )
where

import           Headroom.HeaderFn.UpdateCopyright
import           Headroom.Types                 ( CurrentYear(..) )
import           RIO
import qualified RIO.Text                      as T
import           Test.Hspec


spec :: Spec
spec = do
  let currYear = CurrentYear 2020


  describe "updateYears" $ do
    it "does nothing on up-to-date year" $ do
      let sample = "Copyright (c) 2020"
      updateYears currYear sample `shouldBe` sample

    it "does nothing on up-to-date year range" $ do
      let sample = "Copyright (c) 2018-2020"
      updateYears currYear sample `shouldBe` sample

    it "updates outdated year" $ do
      let sample   = "Copyright (c) 2019"
          expected = "Copyright (c) 2019-2020"
      updateYears currYear sample `shouldBe` expected

    it "updates outdated year range" $ do
      let sample   = "Copyright (c) 2017-2019"
          expected = "Copyright (c) 2017-2020"
      updateYears currYear sample `shouldBe` expected

    it "updates complex multi-line text" $ do
      let sample = T.unlines
            [ "Copyright (c) 2019"
            , "Copyright (c) 2020"
            , "Copyright (c) 2019-2020"
            , "Copyright (c) 2017-2019"
            ]
          expected = T.unlines
            [ "Copyright (c) 2019-2020"
            , "Copyright (c) 2020"
            , "Copyright (c) 2019-2020"
            , "Copyright (c) 2017-2020"
            ]
      updateYears currYear sample `shouldBe` expected
