{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Client where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString            as S
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.HashMap.Strict        as H
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Typeable
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import qualified System.IO.Streams          as Streams

type URL = S.ByteString
type RequestParams = [(S.ByteString, S.ByteString)]

data StripeRequest = StripeRequest
    { method :: Method
    , url    :: S.ByteString
    , params :: RequestParams
    } deriving (Show)

data StripeConfig = StripeConfig
    { publicKey  :: S.ByteString
    , secretKey  :: S.ByteString
    , apiVersion :: S.ByteString
    } deriving (Show)

data StripeErrorType = InvalidRequest | APIError | CardError

-- data StripeError = StripeError {
--       errorType :: StripeErrorType
--     }

sendStripeRequest :: StripeRequest -> StripeConfig -> IO ()
sendStripeRequest StripeRequest{..} StripeConfig{..} = withOpenSSL $ do
  ctx <- baselineContextSSL
  c <- openConnectionSSL ctx "api.stripe.com" 443
  q <- buildRequest $ do
          http method $ "/v1/" <> url
          setAuthorizationBasic secretKey ""
          setContentType "application/x-www-form-urlencoded"
          setHeader "Stripe-Version" apiVersion
  body <- Streams.fromByteString $ convertToString params
  sendRequest c q (inputStreamBody body)
  res <- receiveResponse c $ \p i -> do
         xm <- Streams.read i
         print $ ("code", getStatusCode p)
         print $ ("msg", getStatusMessage p)
         case xm of
             Just x  -> print (decode (strictToLazy x) :: Maybe Object)
             Nothing -> return ()
  print res
  closeConnection c
  return res

lazyToStrict = S.concat . BL.toChunks
strictToLazy = BL.fromChunks . (:[])

convertToString :: RequestParams -> S.ByteString
convertToString ((x,y) : []) = x <> "=" <> y
convertToString ((x,y) : xs) = x <> "=" <> y <> "&" <> convertToString xs

makeRequest :: IO ()
makeRequest = sendStripeRequest req config
  where req = StripeRequest GET "customers" [("expand[]", "customer")]
        config = StripeConfig "" "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

makeBadRequest :: IO ()
makeBadRequest = sendStripeRequest req config
  where req = StripeRequest GET "customersasdfkljsadf" [("expand[]", "customer")]
        config = StripeConfig "" "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

