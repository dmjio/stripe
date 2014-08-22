{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Client.Internal
    ( callAPI
    , runStripe
    , Stripe             (..)
    , StripeRequest      (..)
    , StripeConfig       (..)
    , StripeList         (..)
    , StripeDeleteResult (..)
    , Method             (..)
    ) where

import           Control.Applicative
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString                 (ByteString)
import           Data.Monoid                     ((<>))
import           Data.Text                       (Text)
import           Network.Http.Client
import           OpenSSL                         (withOpenSSL)
import           Web.Stripe.Internal.StripeError
import           Web.Stripe.Util

import qualified Data.ByteString                 as S
import qualified Data.ByteString.Lazy            as BL
import qualified Data.ByteString.Lazy.Char8      as BL8
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import qualified System.IO.Streams               as Streams

-- Base Type we use for Stripe
type Stripe a = ReaderT StripeConfig IO (Either StripeError a)

-- HTTP Params type
type Params = [(ByteString, ByteString)]

data StripeRequest = StripeRequest
    { method :: Method
    , url    :: Text
    , params :: Params
    } deriving (Show)

data StripeConfig = StripeConfig
    { secretKey  :: S.ByteString
    , apiVersion :: S.ByteString
    } deriving (Show)

runStripe :: FromJSON a => StripeConfig -> Stripe a -> IO (Either StripeError a)
runStripe config action = flip runReaderT config action

callAPI :: FromJSON a => StripeRequest -> Stripe a
callAPI request = do
  config <- ask
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
           Streams.read inputStream >>= maybeStream response
      where
        maybeStream response = maybe (error "couldn't read stream") (handleStream response)
        handleStream p x = do
          return $ case getStatusCode p of
                     200 -> maybe (error "Parse failure") Right (decodeStrict x)
                     code | code >= 400 ->
                        do let json = case decodeStrict x :: Maybe StripeError of
                                        Nothing -> error "Parse Failure"
                                        Just x  -> x
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

data StripeList a = StripeList
    { hasMore    :: Bool
    , stripeList :: [a]
    } deriving (Show, Eq)

instance FromJSON a => FromJSON (StripeList a) where
   parseJSON (Object o) =
       StripeList <$> o .: "has_more"
                  <*> o .: "data"

data StripeDeleteResult = StripeDeleteResult {
      deleted   :: Bool
    , deletedId :: Text
    } deriving (Show, Eq)

instance FromJSON StripeDeleteResult where
   parseJSON (Object o) =
       StripeDeleteResult <$> o .: "deleted"
                          <*> o .: "id"
