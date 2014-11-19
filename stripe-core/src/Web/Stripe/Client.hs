{-# LANGUAGE DeriveDataTypeable #-}
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
    , StripeConfig  (..)
    , APIVersion    (..)
    ) where

import           Data.Aeson      (eitherDecodeStrict)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
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
data StripeConfig = StripeConfig
    { secretKey :: ByteString
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

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
    :: (ByteString -> Either String a) -- ^ function to decode JSON result (typically the 'decodeJson' field from 'StripeRequest')
    -> Int                             -- ^ HTTP response code
    -> S.ByteString                    -- ^ HTTP request body
    -> Either StripeError a
handleStream eitherDecodeStrict_a statusCode x =
  case statusCode of
    200 -> case eitherDecodeStrict_a x of
      Left message ->
        -- when debug $ print (eitherDecodeStrict x :: Either String Value)
        parseFail message
      Right json   -> (Right json)
    code | code >= 400 ->
      case eitherDecodeStrict x :: Either String StripeError of
        Left message -> parseFail message
        Right json ->
          Left $ case code of
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
  where
    parseFail errorMessage  =
      Left $ StripeError ParseFailure (T.pack errorMessage) Nothing Nothing Nothing
    unknownCode =
      Left $ StripeError UnknownErrorType mempty Nothing Nothing Nothing
