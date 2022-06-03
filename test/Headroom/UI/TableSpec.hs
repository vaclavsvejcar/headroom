{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Headroom.UI.TableSpec (
    spec
) where

import qualified Headroom.Data.Text as T
import Headroom.UI.Table
import RIO
import Test.Hspec

spec :: Spec
spec = do
    describe "Display instance for Table2" $ do
        it "prints columns correctly aligned" $ do
            let sample =
                    Table2
                        [ ("hello", "world")
                        , ("super super long first column", "foo")
                        , ("bar", "baz")
                        ]
                expected =
                    T.fromLines
                        [ "hello                         world"
                        , "super super long first column foo"
                        , "bar                           baz"
                        ]
            textDisplay sample `shouldBe` expected
