{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Client.HttpClient
       (
         StripeRequest(..)
       , StripeError(..)
       , StripeConfig(..)

       , stripe
       , stripeManager
       , stripeConn

         -- * low-level
       , withConnection
       , withManager
       , callAPI

       ) where

import qualified Control.Arrow
import qualified Data.ByteString.Lazy     as BSL
import           Data.Maybe
import qualified Data.Text.Encoding       as TE
import qualified Network.HTTP.Types       as Http

import Data.Aeson               as A
import Data.ByteString          (ByteString)
import Data.Monoid              ((<>))
#if MIN_VERSION_http_client(0,5,13)
import Network.HTTP.Client      as Http hiding (withManager, withConnection)
#else
import Network.HTTP.Client      as Http hiding (withManager)
#endif
import Network.HTTP.Client.TLS  as TLS

import qualified Web.Stripe.StripeRequest as S

import Web.Stripe.Client (APIVersion (..), StripeConfig (..),
                          StripeError (..), StripeKey (..),
                          defaultEndpoint, Endpoint (..),
                          StripeRequest, StripeReturn,
                          attemptDecode, handleStream,
                          parseFail, toBytestring,
                          unknownCode, Protocol (..))


-- | Create a request to 'Stripe's API.
--
-- This function uses the global TLS manager from @http-client-tls@
-- via 'getGlobalManager'.
stripe :: FromJSON (StripeReturn a)
       => StripeConfig
       -> StripeRequest a
       -> IO (Either StripeError (StripeReturn a))
stripe config request = do
    man <- TLS.getGlobalManager
    callAPI man fromJSON config request

-- | Create a request to 'Stripe's API using a 'Manager'.
stripeManager :: FromJSON (StripeReturn a)
              => Manager
              -> StripeConfig
              -> StripeRequest a
              -> IO (Either StripeError (StripeReturn a))
stripeManager manager config request = callAPI manager fromJSON config request

-- | Create a request to 'Stripe's API using a 'Manager'.
--
-- This function is used to maintain compatibility w/
-- @stripe-http-streams@. However, the terminology in @http-streams@
-- uses 'Connection' whereas @http-client@ uses connection 'Manager'.
stripeConn :: FromJSON (StripeReturn a)
           => Manager
           -> StripeConfig
           -> StripeRequest a
           -> IO (Either StripeError (StripeReturn a))
stripeConn = stripeManager

withConnection :: (Manager -> IO (Either StripeError a))
               -> IO (Either StripeError a)
withConnection = withManager

withManager :: (Manager -> IO (Either StripeError a))
            -> IO (Either StripeError a)
withManager m = do

    -- @http-client@ has a set of deprecated `withManager` functions
    -- that are not necessary to safely prevent a 'Manager' from
    -- leaking resources. "Manager's will be closed and shutdown
    -- automatically (and safely) via gargage collection.
    manager <- TLS.getGlobalManager
    m manager

-- | Create a request to 'Stripe's API using an existing 'Manager'
--
-- This is a low-level function. In most cases you probably want to
-- use 'stripe' or 'stripeManager'.
callAPI :: Manager
        -> (Value -> Result b)
        -> StripeConfig
        -> StripeRequest a
        -> IO (Either StripeError b)
callAPI man fromJSON' config stripeRequest = do

    res <- httpLbs mkStripeRequest man

    let status = Http.statusCode (Http.responseStatus res)

    if not (attemptDecode status) then
        return unknownCode

    else do
        case A.eitherDecode (Http.responseBody res) of
            Left e  -> pure $ parseFail e Nothing
            Right a -> pure $ handleStream fromJSON' status $ return a
  where
    mkStripeRequest =

        let req = Http.applyBasicAuth (getStripeKey (secretKey config)) mempty $
                  defaultRequest {
                    Http.method = m2m (S.method stripeRequest)
                  , Http.secure = endpointProtocol (fromMaybe defaultEndpoint (stripeEndpoint config)) == HTTPS
                  , Http.host = endpointUrl $ fromMaybe defaultEndpoint (stripeEndpoint config)
                  , Http.port = endpointPort $ fromMaybe defaultEndpoint (stripeEndpoint config)
                  , Http.path = "/v1/" <> TE.encodeUtf8 (S.endpoint stripeRequest)
                  , Http.requestHeaders = [
                        ("Stripe-Version", toBytestring stripeVersion)
                      , ("Connection", "Keep-Alive")
                      ]
                  , Http.checkResponse = \_ _ -> return ()
                  }

            stripeQueryParams = fmap
                                  (Control.Arrow.second Just)
                                  (S.queryParams stripeRequest)

        in if S.GET == S.method stripeRequest then
               Http.setQueryString stripeQueryParams req
           else
               urlEncodeBody (S.queryParams stripeRequest) req

m2m :: S.Method -> Http.Method
m2m S.GET    = Http.methodGet
m2m S.POST   = Http.methodPost
m2m S.DELETE = Http.methodDelete

-- | This function is used instead of http-client's built-in 'urlEncodedBody' as
-- the request method is set explicitly to POST in 'urlEncodeBody' but Stripe
-- uses POST\/PUT\/DELETE. A PR should be submitted to http-client to fix
-- eventually.
urlEncodeBody :: [(ByteString, ByteString)] -> Request -> Request
urlEncodeBody headers req = req {
      requestBody = RequestBodyLBS (BSL.fromChunks body)
    , requestHeaders =
        ("Content-Type", "application/x-www-form-urlencoded")
      : filter (\(x, _) -> x /= "Content-Type") (requestHeaders req)
    }
  where
    body = pure (Http.renderSimpleQuery False headers)

stripeVersion :: APIVersion
stripeVersion = V20141007
