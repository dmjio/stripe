{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-- |
-- Module      : Web.Stripe.Error
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Error
    ( -- * Types
      StripeErrorHTTPCode (..)
    , StripeErrorType     (..)
    , StripeErrorCode     (..)
    , StripeError         (..)
    ) where

import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Text           (Text)
import           Data.Typeable

------------------------------------------------------------------------------
-- | Error Codes for HTTP Responses
data StripeErrorHTTPCode =
          BadRequest        -- ^ 400
        | UnAuthorized      -- ^ 401
        | RequestFailed     -- ^ 402
        | NotFound          -- ^ 404
        | StripeServerError -- ^ (>=500)
        | UnknownHTTPCode   -- ^ All other codes
          deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | Stripe Error Types
data StripeErrorType =
          InvalidRequest
        | APIError
        | CardError
        | ConnectionFailure
        | ParseFailure
        | UnknownErrorType
          deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | Stripe Error Codes
data StripeErrorCode =
          IncorrectNumber
        | InvalidNumber
        | InvalidExpiryMonth
        | InvalidExpiryYear
        | InvalidCVC
        | ExpiredCard
        | IncorrectCVC
        | IncorrectZIP
        | CardDeclined
        | Missing
        | ProcessingError
        | RateLimit
        | UnknownError
          deriving (Show, Typeable)

------------------------------------------------------------------------------
-- | Stripe Error
data StripeError = StripeError {
      errorType  :: StripeErrorType
    , errorMsg   :: Text
    , errorCode  :: Maybe StripeErrorCode
    , errorParam :: Maybe Text
    , errorHTTP  :: Maybe StripeErrorHTTPCode
    , errorValue :: Maybe Value
    } deriving (Show, Typeable)

instance Exception StripeError

------------------------------------------------------------------------------
-- | Parses an error message into a `StripeErrorType`
toErrorType
    :: Text
    -> StripeErrorType
toErrorType "invalid_request_error" = InvalidRequest
toErrorType "api_error"             = APIError
toErrorType "card_error"            = CardError
toErrorType _                       = UnknownErrorType

------------------------------------------------------------------------------
-- | Parses an error message into a `StripeErrorCode`
toErrorCode
    :: Text
    -> StripeErrorCode
toErrorCode "incorrect_number"     = IncorrectNumber
toErrorCode "invalid_number"       = InvalidNumber
toErrorCode "invalid_expiry_month" = InvalidExpiryMonth
toErrorCode "invalid_expiry_year"  = InvalidExpiryYear
toErrorCode "invalid_cvc"          = InvalidCVC
toErrorCode "expired_card"         = ExpiredCard
toErrorCode "incorrect_cvc"        = IncorrectCVC
toErrorCode "incorrect_zip"        = IncorrectZIP
toErrorCode "card_declined"        = CardDeclined
toErrorCode "missing"              = Missing
toErrorCode "processing_error"     = ProcessingError
toErrorCode "rate_limit"           = RateLimit
toErrorCode _                      = UnknownError

instance FromJSON StripeError where
    parseJSON (Object o) = do
        e     <- o .: "error"
        typ   <- toErrorType <$> e .: "type"
        msg   <- e .: "message"
        code  <- fmap toErrorCode <$> e .:? "code"
        param <- e .:? "param"
        value <- e .:? "value"
        return $ StripeError typ msg code param Nothing value
    parseJSON _ = mzero
