module Web.Stripe.Token where

import           Control.Applicative             ((<$>), (<*>), pure)
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import           Data.Time
import           Web.Stripe.Client.Internal
import           Web.Stripe.Util
import           Web.Stripe.Internal.StripeError

config :: StripeConfig
config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

newtype TokenId = TokenId Text deriving (Show, Eq, Ord)

data TokenType = TokenCard | TokenBankAccount deriving (Show, Eq)

instance FromJSON TokenType where
   parseJSON (String "bank_account") 
       = pure TokenBankAccount
   parseJSON (String "card")         
       = pure TokenCard
   parseJSON _                       
       = error "Additional token type not documented in Stripe's API"

data Token = Token { 
      tokenId :: TokenId
    , tokenCreated :: UTCTime
    , tokenUsed :: Bool
    , tokenType :: TokenType
    , tokenCard :: Card
} deriving (Show)

instance FromJSON Token where
   parseJSON (Object o) =
       Token <$> (TokenId <$> (o .: "id"))
             <*> o .: "livemode"
             <*> (fromSeconds <$> o .: "created")
             <*> o .: "used"
             <*> o .: "type"
             <*> o .: "card"

instance FromJSON Token where
   parseJSON (Object o) = undefined  

createCardToken :: Stripe Token
createCardToken = callAPI request 
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = [ ("card[number]", "4242424242424242")
                  , ("card[exp_month]", "12")
                  , ("card[exp_year]", "2015")
                  , ("card[cvc]", "123")
                  ]

createBankAccountToken :: Stripe Token
createBankAccountToken = callAPI request 
  where request = StripeRequest POST url params
        url     = "tokens"
        params  = [ ("bank_account[country]", "US")
                  , ("bank_account[routing_number]", "110000000")
                  , ("bank_account[account_number]", "000123456789")
                  ]

getToken :: TokenId -> Stripe Token
getToken (TokenId token) = callAPI request
  where request = StripeRequest GET url params
        url     = "tokens/" <> token
        params  = []






