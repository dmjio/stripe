{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Client.Internal
    ( callAPI
    , runStripe
    , config
    , Stripe             (..)
    , StripeRequest      (..)
    , StripeConfig       (..)
    , StripeDeleteResult (..)
    , Method             (..)
    ) where

import           Control.Applicative             ((<$>), (<*>))
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Reader            (ReaderT, ask, runReaderT)
import           Data.Aeson                      (FromJSON, Value (Object),
                                                  decodeStrict, parseJSON, (.:))
import           Data.Maybe
import           Data.ByteString                 (ByteString)
import           Data.Maybe                      (fromMaybe)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import           Network.Http.Client             (Method(..), baselineContextSSL,
                                                  buildRequest, getStatusCode,
                                                  http, inputStreamBody,
                                                  openConnectionSSL,
                                                  receiveResponse, sendRequest,
                                                  setAuthorizationBasic,
                                                  setContentType, setHeader)
import           OpenSSL                         (withOpenSSL)
import           Web.Stripe.Client.Error         (StripeError (..),
                                                  StripeErrorHTTPCode (..))
import           Web.Stripe.Util                 (paramsToByteString)
import           Web.Stripe.Client.Types 

import qualified Data.ByteString                 as S
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BL8
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified System.IO.Streams               as Streams

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-08-20"

runStripe :: FromJSON a => StripeConfig -> Stripe a -> IO (Either StripeError a)
runStripe = flip runReaderT

callAPI :: FromJSON a => StripeRequest -> Stripe a
callAPI request = ask >>= \config ->
  liftIO $ sendStripeRequest config request

sendStripeRequest :: FromJSON a =>
                     StripeConfig ->
                     StripeRequest ->
                     IO (Either StripeError a)
sendStripeRequest StripeConfig{..} StripeRequest{..} = withOpenSSL $ do
  ctx <- baselineContextSSL
  con <- openConnectionSSL ctx "api.stripe.com" 443
  req <- buildRequest $ do
          http method $ "/v1/" <> T.encodeUtf8 url
          setAuthorizationBasic secretKey ""
          setContentType "application/x-www-form-urlencoded"
          setHeader "Stripe-Version" apiVersion
  body <- Streams.fromByteString $ paramsToByteString params
  
  sendRequest con req $ inputStreamBody body
  receiveResponse con $ \response inputStream ->
           Streams.read inputStream >>= \res -> do
             print (decodeStrict (fromJust res) :: Maybe Value)
             maybeStream response res
  where
    maybeStream response = maybe (error "Couldn't read stream") (handleStream response)
    handleStream p x =
        return $ case getStatusCode p of
                   200 -> maybe (error "Parse failure") Right (decodeStrict x)
                   code | code >= 400 ->
                     do let json = fromMaybe (error "Parse Failure") (decodeStrict x :: Maybe StripeError)
                        Left $ case code of
                                 400 -> json { errorHTTP = BadRequest }
                                 401 -> json { errorHTTP = UnAuthorized }
                                 402 -> json { errorHTTP = RequestFailed }
                                 404 -> json { errorHTTP = NotFound }
                                 500 -> json { errorHTTP = StripeServerError }
                                 502 -> json { errorHTTP = StripeServerError }
                                 503 -> json { errorHTTP = StripeServerError }
                                 504 -> json { errorHTTP = StripeServerError }
                                 _   -> json { errorHTTP = UnknownHTTPCode }
