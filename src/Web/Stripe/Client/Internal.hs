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
    ) where


import           Control.Exception          (SomeException, try)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Reader (ask, runReaderT)
import           Data.Aeson                 (FromJSON, Value, 
                                             decodeStrict)
import           Data.Monoid                (mempty, (<>))
import           Control.Monad              (when)
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
runStripe = flip runReaderT

------------------------------------------------------------------------------
-- | Debug Helper
debug :: Bool
debug = True

------------------------------------------------------------------------------
-- | API request to be issued
callAPI
    :: FromJSON a
    => StripeRequest
    -> Stripe a
callAPI request =
    ask >>= \config ->
        liftIO $ sendStripeRequest config request

------------------------------------------------------------------------------
-- | The guts of issuing an HTTP Request to a `Stripe` API endpoint
sendStripeRequest
    :: FromJSON a
    => StripeConfig
    -> StripeRequest
    -> IO (Either StripeError a)
sendStripeRequest
    StripeConfig {..}
    StripeRequest{..} =
        withOpenSSL $ do
          ctx <- baselineContextSSL
          result <- try (openConnectionSSL ctx "api.stripe.com" 443) :: IO (Either SomeException Connection)
          case result of
            Left  msg -> return $ Left $ StripeError ConnectionFailure (toText msg) Nothing Nothing Nothing
            Right con -> handleConnection con
  where
    handleConnection con = do
      let reqBody | method == GET = mempty
                  | otherwise     = paramsToByteString queryParams
          reqURL  | method == GET = S.concat [ T.encodeUtf8 endpoint
                                             , "?"
                                             , paramsToByteString queryParams
                                             ]
                  | otherwise = T.encodeUtf8 endpoint
      req <- buildRequest $ do
          http method $ "/v1/" <> reqURL
          setAuthorizationBasic secretKey mempty
          setContentType "application/x-www-form-urlencoded"
          setHeader "Stripe-Version" (toBytestring V20141007)
      body <- Streams.fromByteString reqBody
      sendRequest con req $ inputStreamBody body
      json <- receiveResponse con $
              \response inputStream -> do
                  when debug $ print response
                  concatHandler response inputStream >>= \result -> do
                      when debug $ do
                        print result
                        print (decodeStrict result :: Maybe Value)
                      handleStream response result
      closeConnection con
      return json
    parseFail = Left $ StripeError ParseFailure "could not parse response" Nothing Nothing Nothing
    unknownCode = Left $ StripeError UnknownErrorType mempty Nothing Nothing Nothing
    handleStream p x =
        return $
          case getStatusCode p of
            200 -> case decodeStrict x of
                     Nothing -> parseFail
                     Just json -> Right json
            code | code >= 400 ->
                    case decodeStrict x :: Maybe StripeError of
                      Nothing -> parseFail
                      Just json ->
                          Left $ case code of
                             400 -> json { errorHTTP = Just BadRequest }
                             401 -> json { errorHTTP = Just UnAuthorized }
                             402 -> json { errorHTTP = Just RequestFailed }
                             404 -> json { errorHTTP = Just NotFound }
                             500 -> json { errorHTTP = Just StripeServerError }
                             502 -> json { errorHTTP = Just StripeServerError }
                             503 -> json { errorHTTP = Just StripeServerError }
                             504 -> json { errorHTTP = Just StripeServerError }
                             _   -> json { errorHTTP = Just UnknownHTTPCode }
            _ -> unknownCode
