{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Web.Stripe.Client.Internal
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Client.Internal
    ( callAPI
    , stripe
    , Stripe
    , StripeRequest      (..)
    , StripeError        (..)
    , StripeConfig       (..)
    , Method             (..)
    , module Web.Stripe.Client.Util
    , module Web.Stripe.Client.Error
    ) where

import           Control.Exception          (SomeException, try)
import           Control.Monad              (when)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Reader       (ask, runReaderT)
import           Control.Monad.Trans.Either (left, runEitherT, right)
import           Data.Aeson                 (FromJSON, Value(..), eitherDecodeStrict)
import           Data.Monoid                (mempty, (<>))
import qualified Data.Text as T
import           Network.Http.Client        (Connection, Method (..),
                                             baselineContextSSL, buildRequest,
                                             closeConnection, concatHandler, 
                                             getStatusCode, http,
                                             inputStreamBody, openConnectionSSL,
                                             receiveResponse, sendRequest,
                                             setAuthorizationBasic,
                                             setContentType, setHeader)
import           OpenSSL                    (withOpenSSL)
import           Web.Stripe.Client.Error    (StripeError (..),
                                             StripeErrorHTTPCode (..),
                                             StripeErrorType (..))
import           Web.Stripe.Client.Types    (APIVersion (..), Stripe,
                                             StripeConfig (..),
                                             StripeRequest (..))
import           Web.Stripe.Client.Util     (fromSeconds, getParams,
                                             paramsToByteString, toBytestring,
                                             toExpandable, toMetaData, toText,
                                             (</>))

import qualified Data.ByteString            as S
import qualified Data.Text.Encoding         as T
import qualified System.IO.Streams          as Streams

------------------------------------------------------------------------------
-- | Main entry point for beginning a `Stripe` API request
stripe
    :: FromJSON a
    => StripeConfig
    -> Stripe a
    -> IO (Either StripeError a)
stripe config requests = do
  withOpenSSL $ do
    ctx <- baselineContextSSL
    result <- try (openConnectionSSL ctx "api.stripe.com" 443) :: IO (Either SomeException Connection)
    case result of
      Left msg -> return $ Left $ StripeError ConnectionFailure (toText msg) Nothing Nothing Nothing
      Right conn -> do
        json <- flip runReaderT (config, conn) $ runEitherT requests
        closeConnection conn
        return json

------------------------------------------------------------------------------
-- | Debug Helper
debug :: Bool
debug = False

------------------------------------------------------------------------------
-- | API Request to be issued
callAPI :: FromJSON a => StripeRequest -> Stripe a
callAPI StripeRequest{..} = do
  (StripeConfig{..}, conn) <- ask
  let reqBody | method == GET = mempty
              | otherwise     = paramsToByteString queryParams
      reqURL  | method == GET = S.concat [
                  T.encodeUtf8 endpoint
                  , "?"
                  , paramsToByteString queryParams
                  ]
              | otherwise = T.encodeUtf8 endpoint
  handleStream =<< liftIO (do
    req <- buildRequest $ do
      http method $ "/v1/" <> reqURL
      setAuthorizationBasic secretKey mempty
      setContentType "application/x-www-form-urlencoded"
      setHeader "Stripe-Version" (toBytestring V20141007)
      setHeader "Connection" "Keep-Alive"
    body <- Streams.fromByteString reqBody
    sendRequest conn req $ inputStreamBody body
    receiveResponse conn $ \response inputStream ->
      do when debug $ print response
         result <- concatHandler response inputStream
         return (response, result))
  where
    parseFail errorMessage  = 
      left $ StripeError ParseFailure (T.pack errorMessage) Nothing Nothing Nothing
    unknownCode = left $ StripeError UnknownErrorType mempty Nothing Nothing Nothing
    handleStream (p,x) =
          case getStatusCode p of
            200 -> case eitherDecodeStrict x of
                     Left message -> do
                       when debug $ liftIO $ print (eitherDecodeStrict x :: Either String Value)
                       parseFail message
                     Right json   -> right json
            code | code >= 400 ->
                    case eitherDecodeStrict x :: Either String StripeError of
                      Left message -> parseFail message
                      Right json ->
                          left $ case code of
                             400 -> json { errorHTTP = Just BadRequest        }
                             401 -> json { errorHTTP = Just UnAuthorized      }
                             402 -> json { errorHTTP = Just RequestFailed     }
                             404 -> json { errorHTTP = Just NotFound          }
                             500 -> json { errorHTTP = Just StripeServerError }
                             502 -> json { errorHTTP = Just StripeServerError }
                             503 -> json { errorHTTP = Just StripeServerError }
                             504 -> json { errorHTTP = Just StripeServerError }
                             _   -> json { errorHTTP = Just UnknownHTTPCode   }
            _ -> unknownCode
