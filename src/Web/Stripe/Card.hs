{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}

module Web.Stripe.Card 
    ( Card(..)
    ) where

import           Control.Applicative ((<*>), (<$>))
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Time
import           Network.Http.Client
import           Web.Stripe.Client.Internal
import           Web.Stripe.Customer
import           Web.Stripe.Util

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

-- | Create card
addCardToCustomer :: CustomerId -> TokenId -> Stripe Customer
addCardToCustomer (CustomerId cid) (TokenId tokenId) =
    callAPI request
  where request = StripeRequest POST url params
        url     = "customers/" <> cid <> "/cards"
        params  = [("card", toBS tokenId)]

-- | Create a Stripe token from a credit card
createCardToken :: CardNumber ->
                   ExpMonth ->
                   ExpYear ->
                   CVC ->
                   Stripe Token
createCardToken (CardNumber num) (ExpMonth month) (ExpYear year) (CVC cvc)
    = callAPI request
  where request = StripeRequest POST "tokens" params
        url     = "tokens"
        params  = [ ("card[number]", toBS num)
                  , ("card[exp_month]", toBS month)
                  , ("card[exp_year]", toBS year)
                  , ("card[cvc]", toBS cvc)
                  ]

-- | Get card by CustomerID and CardID
getCard :: CustomerId -> CardId -> Stripe Card
getCard (CustomerId custId) (CardId cardId) = callAPI request
  where request = StripeRequest GET url params
        url     = "customers/" <> custId <> "/cards/" <> cardId
        params  = []

updateCard :: CustomerId ->
              CardId ->
              Maybe AddressCity -> -- addressCity
              Maybe AddressCountry -> -- addressCountry
              Maybe AddressLine1 -> -- addressLine1
              Maybe AddressLine2 -> -- addressLine2
              Maybe AddressState -> -- addressState
              Maybe AddressZip -> -- addressZip
              Maybe ExpMonth -> -- expMonth
              Maybe ExpYear -> -- expYear
              Maybe Name -> -- name
              Stripe Card
updateCard (CustomerId custId)
           (CardId cardId)
           addressCity
           addressCountry
           addressLine1
           addressLine2
           addressState
           addressZip
           expMonth
           expYear
           name = callAPI request
  where request = StripeRequest POST url params
        url     = "customers/" <> custId <> "/cards/" <> cardId
        params  = [ (k, v) | (k, Just v) <- [
                     ("address_city",  (\(AddressCity x) -> toBS x) <$> addressCity)
                   , ("address_country", (\(AddressCountry x) -> toBS x) <$> addressCountry)
                   , ("address_line1", (\(AddressLine1 x) -> toBS x) <$> addressLine1 )
                   , ("address_line2", (\(AddressLine2 x) -> toBS x) <$> addressLine2 )
                   , ("address_state", (\(AddressState x) -> toBS x) <$> addressState )
                   , ("address_zip", (\(AddressZip x) -> toBS x) <$> addressZip )
                   , ("exp_month", (\(ExpMonth x) -> toBS x) <$> expMonth )
                   , ("exp_year", (\(ExpYear x) -> toBS x) <$> expYear )
                   , ("name", (\(Name x) -> toBS x) <$> name )
                   ]
                 ]

deleteCard :: CustomerId -> CardId -> Stripe StripeDeleteResult
deleteCard (CustomerId custId) (CardId cardId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = "customers/" <> custId <> "/cards/" <> cardId
        params  = []
