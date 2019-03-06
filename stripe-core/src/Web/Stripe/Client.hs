{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-- |
-- Module      : Web.Stripe.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Client
    ( module Web.Stripe.StripeRequest
    , module Web.Stripe.Error
    , module Web.Stripe.Util
    , handleStream
    , parseFail
    , attemptDecode
    , unknownCode
    , defaultEndpoint
    , Endpoint      (..)
    , StripeConfig  (..)
    , StripeKey     (..)
    , APIVersion    (..)
    ) where

import           Data.Aeson      (Value, Result(..), fromJSON)
import           Data.ByteString (ByteString)
import           Data.Data       (Data, Typeable)
import           Data.Monoid     (mempty)
import           Data.Text       as T
import           Text.Read       (lexP, pfail)
import qualified Text.Read       as R
import           Web.Stripe.StripeRequest
import           Web.Stripe.Error
import           Web.Stripe.Util


------------------------------------------------------------------------------
-- | Stripe secret key
newtype StripeKey = StripeKey
    { getStripeKey :: ByteString
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Stripe config
data StripeConfig = StripeConfig
    { secretKey :: StripeKey
    , stripeEndpoint :: Maybe Endpoint
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Stripe endpoint, useful for mocking
newtype Endpoint = Endpoint { getEndpoint :: ByteString }
  deriving (Show, Eq, Read, Ord, Data)

defaultEndpoint :: Endpoint
defaultEndpoint = Endpoint "api.stripe.com"

------------------------------------------------------------------------------
-- | API Version
data APIVersion =
    V20141007 -- ^ Stripe API Version for this package release
    deriving (Eq, Ord, Data, Typeable)

instance Show APIVersion where
    show V20141007 = "2014-10-07"

instance Read APIVersion where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "2014-10-07" -> return V20141007
         _            -> pfail

------------------------------------------------------------------------------
-- | handleStream
--
-- This function is used by the backends such as @stripe-http-client@ to
-- decode the results of an API request.
handleStream
    :: (Value -> Result a)             -- ^ function to decode JSON value
    -> Int                             -- ^ HTTP response code
    -> Result Value                    -- ^ result of attempting to decode body
    -> Either StripeError a
handleStream decodeValue statusCode r =
  case statusCode of
    200 -> case r of
      Error message -> parseFail message
      (Success value) ->
        case decodeValue value of
          (Error message) -> parseFail message
          (Success a)     -> (Right a)
    code | code >= 400 ->
      case r of
      Error message -> parseFail message
      (Success value) ->
        case fromJSON value of
          (Error message) -> parseFail message
          (Success stripeError) ->
            Left $ setErrorHTTP code stripeError
    _ -> unknownCode

------------------------------------------------------------------------------
-- | check the HTTP status code and see if it is one we can deal with or not
attemptDecode
    :: Int  -- ^ HTTP status code
    -> Bool
attemptDecode code = code == 200 || code >= 400

------------------------------------------------------------------------------
-- | lift a parser error to be a StripeError
parseFail
    :: String  -- ^ error message
    -> Either StripeError a
parseFail errorMessage  =
      Left $ StripeError ParseFailure (T.pack errorMessage) Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | `StripeError` to return when we don't know what to do with the
-- received HTTP status code.
unknownCode :: Either StripeError a
unknownCode =
      Left $ StripeError UnknownErrorType mempty Nothing Nothing Nothing

------------------------------------------------------------------------------
-- | set the `errorHTTP` field of the `StripeError` based on the HTTP
-- response code.
setErrorHTTP
  :: Int          -- ^ HTTP Status code
  -> StripeError  -- ^ `StripeError`
  -> StripeError
setErrorHTTP statusCode stripeError =
  case statusCode of
    400 -> stripeError { errorHTTP = Just BadRequest        }
    401 -> stripeError { errorHTTP = Just UnAuthorized      }
    402 -> stripeError { errorHTTP = Just RequestFailed     }
    404 -> stripeError { errorHTTP = Just NotFound          }
    500 -> stripeError { errorHTTP = Just StripeServerError }
    502 -> stripeError { errorHTTP = Just StripeServerError }
    503 -> stripeError { errorHTTP = Just StripeServerError }
    504 -> stripeError { errorHTTP = Just StripeServerError }
    _   -> stripeError { errorHTTP = Just UnknownHTTPCode   }
