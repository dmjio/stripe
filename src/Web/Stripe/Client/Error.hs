{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Client.Error where

import           Control.Applicative
import           Data.Aeson
import           Data.Text           (Text)
import qualified Data.Text           as T

data StripeErrorHTTPCode = 
          BadRequest        -- * 400
        | UnAuthorized      -- * 401
        | RequestFailed     -- * 402
        | NotFound          -- * 404
        | StripeServerError -- * (>500)
        | UnknownHTTPCode   -- * All other codes
          deriving Show

data StripeErrorType =
          InvalidRequest
        | APIError
        | CardError
        | UnknownErrorType 
          deriving Show

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
          deriving Show

data StripeError = StripeError {
      errorType  :: StripeErrorType
    , errorMsg   :: Text
    , errorCode  :: Maybe StripeErrorCode
    , errorParam :: Maybe Text
    , errorHTTP  :: StripeErrorHTTPCode
    } deriving Show

toErrorType :: Text -> StripeErrorType
toErrorType "invalid_request_error" = InvalidRequest
toErrorType "api_error"             = APIError
toErrorType "card_error"            = CardError
toErrorType _                       = UnknownErrorType

toErrorCode :: Text -> StripeErrorCode
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

instance FromJSON StripeError where
    parseJSON (Object o) = do
        e     <- o .: "error"
        typ   <- toErrorType <$> e .: "type"
        msg   <- e .: "message"
        code  <- fmap toErrorCode <$> e .:? "code"
        param <- e .:? "param"
        return $ StripeError typ msg code param UnknownHTTPCode

