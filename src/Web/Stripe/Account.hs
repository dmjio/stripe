{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Account
    ( AccountId(..)
    , Account(..)
    , getAccountDetails
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Aeson
import           Data.Text           (Text)
import           Web.Stripe.Client
import           Web.Stripe.Internal.StripeError

newtype AccountId = AccountId Text deriving (Show, Eq)

data Account = Account {
      accountId                   :: AccountId
     , accountEmail               :: Text
     , accountStatementDescriptor :: Maybe Text
     , accountDisplayName         :: Text
     , accountTimeZone            :: Text
     , accountDetailsSubmitted    :: Bool
     , accountChargeEnabled       :: Bool
     , accountTransferEnabled     :: Bool
     , accountCurrenciesSupported :: [Text]
     , accountDefaultCurrency     :: Text
     , accountCountry             :: Text
} deriving (Show, Eq)

instance FromJSON Account where
   parseJSON (Object o) =
       Account <$> (AccountId <$> o .:  "id")
               <*> o .:  "email"
               <*> o .:? "statement_descriptor"
               <*> o .:  "display_name"
               <*> o .:  "timezone"
               <*> o .:  "details_submitted"
               <*> o .:  "charge_enabled"
               <*> o .:  "transfer_enabled"
               <*> o .:  "currencies_supported"
               <*> o .:  "default_currency"
               <*> o .:  "country"

getAccountDetails :: StripeConfig -> IO (Either StripeError Account)
getAccountDetails config = sendStripeRequest config req params
  where req    = StripeRequest GET "account"
        params = []
