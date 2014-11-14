-- |
-- Module      : Web.Stripe.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Client
    ( module Web.Stripe.Client.Types
    , module Web.Stripe.Client.Error
    , module Web.Stripe.Client.Util
    , handleStream
    ) where

import           Data.Aeson      (eitherDecodeStrict)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Monoid     (mempty)
import qualified Data.Text       as T
import           Web.Stripe.Client.Types
import           Web.Stripe.Client.Error
import           Web.Stripe.Client.Util

handleStream
    :: (ByteString -> Either String a)
    -> Int             -- ^ HTTP response code
    -> S.ByteString    -- ^ HTTP request body
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
