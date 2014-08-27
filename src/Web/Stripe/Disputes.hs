module Web.Stripe.Disputes where

data Dispute = Dispute {
      
} deriving (Show)

instance FromJSON Dispute where
    parseJSON (Object o) = undefined

updateDispute :: ChargeId -> Stripe Dispute
updateDispute (ChargeId chargeId) = callAPI config req []
  where req = StripeRequest POST url
        url = "charges/" <> chargeId <> "/dispute"

closeDispute :: ChargeId -> Stripe Dispute
closeDispute (ChargeId chargeId) = callAPI config req []
  where req = StripeRequest POST url
        url = "charges/" <> chargeId <> "/dispute/close"
