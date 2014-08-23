module Web.Stripe.Transfers where



newtype RecipientId = RecipientId { recipientId :: Text } deriving (Show, Eq)
newtype TransferId = TransferId Text deriving (Show, Eq)

data TransferStatus = Paid | Pending | Canceled | Failed deriving (Show, Eq)
data TransferType = CardTransfer | BankAccountTransfer deriving (Show, Eq)

instance FromJSON TransferType where
    parseJSON (String "card")         = pure CardTransfer
    parseJSON (String "bank_account") = pure BankAccountTransfer

instance FromJSON TransferStatus where
    parseJSON (String "paid")     = pure Paid
    parseJSON (String "pending")  = pure Pending
    parseJSON (String "canceled") = pure Canceled
    parseJSON (String "failed")   = pure Failed

data Transfer = Transfer {
      transferId :: TransferId
    , transferCreated :: UTCTime
    , transferDate :: UTCTime
    , transferAmount :: Int
    , transferCurrency :: Text
    , transferStatus :: TransferStatus
    , transferType :: TransferType
    , transferBalanceTransaction :: Text -- what??
    , transferDescription :: Text
} deriving (Show)

instance FromJSON Transfer where
    parseJSON (Object o) = undefined

createTransfer :: RecipientId -> IO (Either StripeError Transfer)
createTransfer (RecipientId recipientId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers"
        params  = [ ("amount", "400")
                  , ("currency", "usd")
                  , ("recipient", T.encodeUtf8 recipientId)
                  ]

getTransfer :: TransferId -> IO (Either StripeError Transfer)
getTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest GET url params
        url = "transfers/" <> transferId

updateTransfer :: TransferId -> IO (Either StripeError Transfer)
updateTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers/" <> transferId
        params  = []

cancelTransfer :: TransferId -> IO (Either StripeError Transfer)
cancelTransfer (TransferId transferId) = callAPI request
  where request = StripeRequest POST url params
        url     = "transfers/" <> transferId <> "/cancel"
        params  = []