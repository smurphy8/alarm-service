{-# LANGUAGE  OverloadedStrings #-}
module Plow.Service.AlarmSpec (main, spec) where

import Test.Hspec
import Data.Text

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "someFunction" $ do
    it "should work fine" $ do
      True `shouldBe` False






getCEAPostExtCreateBroadcastResult :: Text
getCEAPostExtCreateBroadcastResult = "<?xml version=\"1.0\" encoding=\"utf-8\"?><soap:Envelope xmlns:soap=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"><soap:Body><ExtCreateBroadcastResponse xmlns=\"http://call-em-all.com/\"><ExtCreateBroadcastResult><MaxCallUnitsUsed>1</MaxCallUnitsUsed><MaxMessageLength>5</MaxMessageLength><badRecordCountOnFile>0</badRecordCountOnFile><broadcastID>2596388</broadcastID><duplicateRecrodCountOnFile>0</duplicateRecrodCountOnFile><errorCode>0</errorCode><errorMessage>Operation Successful : </errorMessage><goodRecordCountOnFile>1</goodRecordCountOnFile><messageRecordingID>118015</messageRecordingID><smsRecordCountOnFile>0</smsRecordCountOnFile><tollFreeNumber>8775658460</tollFreeNumber><totalRecordCountOnFile>1</totalRecordCountOnFile><voiceRecordCountOnFile>0</voiceRecordCountOnFile><duplicateRecordCountOnFile>0</duplicateRecordCountOnFile></ExtCreateBroadcastResult></ExtCreateBroadcastResponse></soap:Body></soap:Envelope>"
