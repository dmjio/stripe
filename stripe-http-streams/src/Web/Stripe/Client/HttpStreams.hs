{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- |
-- Module      : Web.Stripe.Client.Internal
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Client.HttpStreams
    ( callAPI
    , stripe
    , withConnection
    , StripeRequest      (..)
    , StripeError        (..)
    , StripeConfig       (..)
    ) where

import           Control.Exception          (SomeException, finally, try)
import           Control.Monad              (when)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as S
import           Data.Monoid                (mempty, (<>))
import qualified Data.Text.Encoding         as T
import qualified Data.Text as T
import           Network.Http.Client        (Connection,
                                             baselineContextSSL, buildRequest,
                                             closeConnection, concatHandler,
                                             getStatusCode, http,
                                             inputStreamBody, openConnectionSSL,
                                             receiveResponse, sendRequest,
                                             setAuthorizationBasic,
                                             setContentType, setHeader)
import qualified Network.Http.Client        as C
import           OpenSSL                    (withOpenSSL)
import qualified System.IO.Streams          as Streams
import           Web.Stripe.Client          (APIVersion (..), Method(..), StripeConfig (..),
                                             StripeError (..), StripeErrorHTTPCode (..),
                                             StripeErrorType (..), StripeRequest (..),
                                             StripeReturn, getStripeKey, handleStream,
                                             toBytestring, toText, paramsToByteString
                                             )

------------------------------------------------------------------------------
-- | Create a request to `Stripe`'s API
stripe
    :: StripeConfig
    -> StripeRequest a
    -> IO (Either StripeError (StripeReturn a))
stripe config request =
  withConnection $ \conn -> do
    callAPI conn config request

------------------------------------------------------------------------------
-- | Open a connection to the stripe API server
withConnection :: (Connection -> IO (Either StripeError a))
               -> IO (Either StripeError a)
withConnection f =
  withOpenSSL $ do
    ctx <- baselineContextSSL
    result <- try (openConnectionSSL ctx "api.stripe.com" 443) :: IO (Either SomeException Connection)
    case result of
      Left msg -> return $ Left $ StripeError ConnectionFailure (toText msg) Nothing Nothing Nothing
      Right conn -> (f conn) `finally` (closeConnection conn)

------------------------------------------------------------------------------
-- | Debug Helper
debug :: Bool
debug = False

------------------------------------------------------------------------------
-- | convert from stripe-core Method type to http-stream Method type
m2m :: Method -> C.Method
m2m GET    = C.GET
m2m POST   = C.POST
m2m DELETE = C.DELETE

------------------------------------------------------------------------------
-- | Create a request to `Stripe`'s API over an existing connection
--
-- see also: 'withConnection'
callAPI
    :: Connection                      -- ^ an open connection to the server (`withConnection`)
    -> StripeConfig                    -- ^ StripeConfig
    -> StripeRequest a                 -- ^ StripeRequest
    -> IO (Either StripeError (StripeReturn a))
callAPI conn StripeConfig {..} StripeRequest{..} = do
  let reqBody | method == GET = mempty
              | otherwise     = paramsToByteString queryParams
      reqURL  | method == GET = S.concat [
                  T.encodeUtf8 endpoint
                  , "?"
                  , paramsToByteString queryParams
                  ]
              | otherwise = T.encodeUtf8 endpoint
  req <- buildRequest $ do
    http (m2m method) $ "/v1/" <> reqURL
    setAuthorizationBasic (getStripeKey secretKey) mempty
    setContentType "application/x-www-form-urlencoded"
    setHeader "Stripe-Version" (toBytestring V20141007)
    setHeader "Connection" "Keep-Alive"
  body <- Streams.fromByteString reqBody
  sendRequest conn req $ inputStreamBody body
  receiveResponse conn $ \response inputStream ->
      do when debug $ print response
         result <- concatHandler response inputStream
         return $ handleStream decodeJson (getStatusCode response) result

         
--         result <- concatHandler response inputStream
--         undefined
{-         
         case getStatusCode response of
           200 ->
             do v <- Streams.parseFromJSON decodeJson inputStream
             case v of
               (Left err) -> ..
               (Right obj) ->
                 return obj
           _ ->
             undefined

--             handleError (getStatusCode response) result


--         result <- concatHandler response inputStream
-}
