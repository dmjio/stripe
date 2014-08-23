module Web.Stripe.Recipient where

import Data.Text.Encoding as T
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

data RecipientType = Individual | Corporation deriving (Eq, Show)

data Recipient = Recipient { 

} deriving (Show)

instance FromJSON Recipient where
   parseJSON (Object o) = undefined

-- Text should be name?
createRecipient :: Text -> RecipientType -> IO (Either StripeError Recipient)
createRecipient name recipientType  = callAPI request
  where request = StripeRequest POST url params
        url     = "recipients"
        params  = [ ("name", T.encodeUtf8 name)
                  , ("type", if recipientType == Individual
                             then "individual"
                             else "corporation")
                  ]

getRecipient :: RecipientId -> IO (Either StripeError Recipient)
getRecipient (RecipientId recipientId) = callAPI config req []
  where req =  StripeRequest GET url 
        url = "recipients/" <> recipientId

-- -- see optional
updateRecipient :: RecipientId -> IO (Either StripeError Recipient)
updateRecipient (RecipientId recipientId) = callAPI config req []
  where req = StripeRequest POST url
        url = "recipients/" <> recipientId

deleteRecipient :: RecipientId -> IO (Either StripeError Recipient)
deleteRecipient (RecipientId recipientId) = callAPI config req []
  where req =  StripeRequest DELETE url 
        url = "recipients/" <> recipientId
