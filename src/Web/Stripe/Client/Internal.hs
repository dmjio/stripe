{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Client.Internal
    ( callAPI
    , runStripe
    , Stripe
    , StripeRequest (..)
    , StripeConfig (..)
    , StripeList(..)
    , StripeResult (..)
    , Method(GET, POST, DELETE, PUT)
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

type URL    = S.ByteString
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

type Stripe a = ReaderT StripeConfig IO (Either StripeError a)

runStripe :: FromJSON a => StripeConfig -> StripeRequest -> IO (Either StripeError a)
runStripe config request = runReaderT (callAPI request) config

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
  c <- openConnectionSSL ctx "api.stripe.com" 443
  q <- buildRequest $ do
          http method $ "/v1/" <> T.encodeUtf8 url
          setAuthorizationBasic secretKey ""
          setContentType "application/x-www-form-urlencoded"
          setHeader "Stripe-Version" apiVersion
  print q
  body <- Streams.fromByteString $ paramsToByteString params
  print $ paramsToByteString params
  sendRequest c q (inputStreamBody body)
  receiveResponse c $ \response inputStream ->
           Streams.read inputStream >>=
                  maybe (error "couldn't read stream") (handleStream response)
      where
        handleStream p x = do
          print (x, p)
          return $ case getStatusCode p of
                     c | c == 200 -> case decodeStrict x of
                                       Nothing -> error "oops"
                                       Just res -> Right res
                       | c >= 400 -> case decodeStrict x :: Maybe StripeError of
                                       Nothing -> error "oops"
                                       Just res  -> Left res
                       | otherwise -> undefined

data StripeList a = StripeList
    { hasMore    :: Bool
    , stripeList :: [a]
    } deriving (Show, Eq)

instance FromJSON a => FromJSON (StripeList a) where
   parseJSON (Object o) =
       StripeList <$> o .: "has_more"
                  <*> o .: "data"

data StripeResult = StripeResult { deleted :: Bool, deletedId :: Text } deriving (Show, Eq)

instance FromJSON StripeResult where
   parseJSON (Object o) =
       StripeResult <$> o .: "deleted"
                    <*> o .: "id"
