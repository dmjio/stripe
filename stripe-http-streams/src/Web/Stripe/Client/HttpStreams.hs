{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies   #-}
-- |
-- Module      : Web.Stripe.Client.Internal
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Client.HttpStreams
    ( stripe
    , stripeConn
    , withConnection
    , StripeRequest      (..)
    , StripeError        (..)
    , StripeConfig       (..)
    -- * low-level
    , callAPI
    ) where

import           Control.Exception          (SomeException, finally, try)
import           Control.Monad              (when)
import           Data.Aeson                 (Result(..), FromJSON, Value, fromJSON, json')
import qualified Data.ByteString            as S
import           Data.Monoid                (mempty, (<>))
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.Encoding         as T
import           Network.Http.Client        (Connection,
                                             baselineContextSSL, buildRequest,
                                             closeConnection,
                                             getStatusCode, http,
                                             inputStreamBody, openConnectionSSL,
                                             receiveResponse, sendRequest,
                                             setAuthorizationBasic, encodedFormBody,
                                             setContentType, setHeader, 
                                             setTransferEncoding)
import qualified Network.Http.Client        as C
import           OpenSSL                    (withOpenSSL)
import qualified System.IO.Streams          as Streams
import qualified System.IO.Streams.Attoparsec as Streams
import           System.IO.Streams.Attoparsec (ParseException(..))
import           Web.Stripe.Client          (APIVersion (..), Method(..), StripeConfig (..),
                                             StripeError (..), defaultEndpoint, Endpoint (..),
                                             StripeErrorType (..), StripeRequest (..),
                                             StripeReturn, getStripeKey,
                                             toBytestring, toText,
                                             paramsToByteString, attemptDecode, unknownCode,
                                             handleStream
                                             )

------------------------------------------------------------------------------
-- | Create a request to `Stripe`'s API
stripe
    :: (FromJSON (StripeReturn a)) =>
       StripeConfig
    -> StripeRequest a
    -> IO (Either StripeError (StripeReturn a))
stripe config request =
  withConnection (fromMaybe defaultEndpoint (stripeEndpoint config)) $ \conn -> do
    stripeConn conn config request

------------------------------------------------------------------------------
-- | Create a request to `Stripe`'s API using a connection opened
-- with `withConnection`
stripeConn
    :: (FromJSON (StripeReturn a)) =>
       Connection
    -> StripeConfig
    -> StripeRequest a
    -> IO (Either StripeError (StripeReturn a))
stripeConn conn config request =
    callAPI conn fromJSON config request

------------------------------------------------------------------------------
-- | Open a connection to the stripe API server
withConnection :: Endpoint -> (Connection -> IO (Either StripeError a))
               -> IO (Either StripeError a)
withConnection (Endpoint endpoint) f =
  withOpenSSL $ do
    ctx <- baselineContextSSL
    result <- try (openConnectionSSL ctx endpoint 443) :: IO (Either SomeException Connection)
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
-- FIXME: all connection errors should be
-- turned into a `StripeError`. But that is not yet implemented.
--
-- NOTES: this a pretty low-level function. You probably want `stripe`
-- or `stripeConn`. If you call this function directly, you are
-- responsible for ensuring the JSON conversion function supplied is
-- correct for `StripeRequest`. In the rest of the library this
-- property is enforced automatically by the type-system. But adding
-- that constraint here made implementing the `Stripe` testing monad
-- difficult.
callAPI
    :: Connection                      -- ^ an open connection to the server (`withConnection`)
    -> (Value -> Result b)             -- ^ function to convert JSON result to Haskell Value
    -> StripeConfig                    -- ^ StripeConfig
    -> StripeRequest a                 -- ^ StripeRequest
    -> IO (Either StripeError b)
callAPI conn fromJSON' StripeConfig {..} StripeRequest{..} = do
  let reqBody | method == GET = mempty
              | otherwise     = queryParams
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
    setTransferEncoding
  sendRequest conn req (encodedFormBody reqBody)
  receiveResponse conn $ \response inputStream ->
      do when debug $ print response
         let statusCode = getStatusCode response
         if not (attemptDecode statusCode)
           then return unknownCode
           else do -- FIXME: should we check the content-type instead
                   -- assuming it is application/json? --DMJ: Stripe
                   -- gaurantees it to be JSON 
                   v <- try (Streams.parseFromStream json' inputStream)
                   let r =
                         case v of
                           (Left (ParseException msg)) -> Error msg
                           (Right a) -> Success a
                   return $ handleStream fromJSON' statusCode r
