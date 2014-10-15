{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Stripe.Client.Internal
    ( callAPI
    , runStripe
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
import           Data.Aeson                 (FromJSON, Value, decodeStrict)
import           Data.Monoid                (mempty, (<>))
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
runStripe
    :: FromJSON a
    => StripeConfig
    -> Stripe a
    -> IO (Either StripeError a)
runStripe config requests = do
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
    receiveResponse conn $ do
      \response inputStream -> do
        when debug $ print response
        concatHandler response inputStream >>= \result -> do
          when debug $ do
            print result
            print (decodeStrict result :: Maybe Value)
          return (response, result))
  where
    parseFail   = left $ StripeError ParseFailure "could not parse response" Nothing Nothing Nothing
    unknownCode = left $ StripeError UnknownErrorType mempty Nothing Nothing Nothing
    handleStream (p,x) =
          case getStatusCode p of
            200 -> case decodeStrict x of
                     Nothing -> parseFail
                     Just json -> right json
            code | code >= 400 ->
                    case decodeStrict x :: Maybe StripeError of
                      Nothing -> parseFail
                      Just json ->
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
