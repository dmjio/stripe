{-# LANGUAGE OverloadedStrings #-}
module Test.Raw (rawTest) where

import           Data.Aeson
import           Data.Either
import           Data.Monoid         ((<>))
import           Data.Text           (Text)
import qualified Data.Text.Encoding  as T
import           Network.Http.Client
import           Test.Hspec
import           Web.Stripe

rawTest :: StripeConfig -> Spec
rawTest config = do
  describe "Raw tests" $ do
    it "Succesfully retrieves account information" $ do
      result <- (stripeRaw config req) :: IO (Either StripeError Value)
      result `shouldSatisfy` isRight
    it "Succesfully encodes x-www-form-urlencoded data" $ do
      let pid = "encoded"
      -- Add form data to the request with special characters
      result <- (stripeRaw config (req2 pid)) :: IO (Either StripeError Value)
      result `shouldSatisfy` isRight
      del <- (stripeRaw config (delReq2 pid)) :: IO (Either StripeError Value)
      del `shouldSatisfy` isRight
  where
    req :: StripeRequest
    req = StripeRequest GET "events" []
    req2 :: Text -> StripeRequest
    req2 pid = StripeRequest POST "plans" [ ("amount", "2000")
                                          , ("interval", "month")
                                          , ("name","<Acme & Co. Gold Plan+>" )
                                          , ("currency","usd" )
                                          , ("id", T.encodeUtf8 pid)
                                          ]
    delReq2 :: Text -> StripeRequest
    delReq2 pid = StripeRequest DELETE ("plans/" <> pid) []
