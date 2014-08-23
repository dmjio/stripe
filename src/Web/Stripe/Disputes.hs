module Web.Stripe.Disputes where

data Dispute = Dispute {
      
} deriving (Show)

instance FromJSON Dispute where
    parseJSON (Object o) = undefined

updateDispute :: ChargeId -> IO (Either StripeError Dispute)
updateDispute (ChargeId chargeId) = callAPI config req []
  where req = StripeRequest POST url
        url = "charges/" <> chargeId <> "/dispute"

closeDispute :: ChargeId -> IO (Either StripeError Dispute)
closeDispute (ChargeId chargeId) = callAPI config req []
  where req = StripeRequest POST url
        url = "charges/" <> chargeId <> "/dispute/close"
