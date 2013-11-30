{-# LANGUAGE OverloadedStrings #-}
module Plow.Service.Alarm.TypesSpec (main, spec) where

import Test.Hspec
-- import Prelude
import Plow.Service.Alarm.Types 

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False
