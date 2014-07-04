module Web.Stripe.Charge where

-- charges, lots of optional fields as well
newtype Charge = Charge Text deriving (Show, Eq)
newtype RefundId = RefundId { refundId :: Text } deriving (Show, Eq)

-- either customer Id or charge is required
data ChargeOptions = ChargeOptions {
      chargeAmount :: Int
    , chargeCurrency :: Text
    , chargeCustomerId :: Maybe Text
    -- , chargeCardId :: Maybe Text
    , chargeDescription :: Maybe Text
    , 
    , 
    } deriving (Show, Eq)

instance URLEncodeable ChargeOptions where
    formEncode ChargeOptions = 
        [ (a, b) | (a, Just b) <- [
           ("amount", Just $ toBS chargeAmount)
         , ("description", fmap toBS customerDescriptonOptions)
         , ("email", fmap toBS customerEmailOptions)
         , ("quantity", fmap toBS customerQuantityOptions)
         , ("trial_end", fmap toBS customerTrialEndOptions)
         ]
        ]


charge :: Either Card CustomerId -> IO ()
charge param = sendStripeRequest req config
  where req = StripeRequest POST "charges" params
        params = result : [ ("amount", "400")
                          , ("currency", "usd")
                          ]
        result = case param of
                   Right (CustomerId custId) ->
                       ("customer", T.encodeUtf8 custId)
                   Left (Card cardId) ->
                       ("card", T.encodeUtf8 cardId)

chargeByCardId :: Card -> IO ()
chargeByCardId card = charge $ Left card

chargeByCustomerId :: CustomerId -> IO ()
chargeByCustomerId customerId = charge $ Right customerId

getCharge :: Charge -> IO ()
getCharge (Charge charge) = sendStripeRequest req config
  where req = StripeRequest GET url []
        url = "charges/" <> charge

-- description + metadata == options
updateCharge :: Charge -> IO ()
updateCharge (Charge charge) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "charges/" <> charge

-- optional params
captureCharge :: Charge -> IO ()
captureCharge (Charge charge) = sendStripeRequest req config
  where req = StripeRequest POST url []
        url = "charges/" <> charge <> "/capture"

-- optional params
getCharges :: IO ()
getCharges = sendStripeRequest req config
  where req = StripeRequest GET "charges" []
--


