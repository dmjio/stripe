{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Card where

import           Control.Applicative
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as T
import           Data.Time
import           Network.Http.Client
import           Web.Stripe.Client
import           Web.Stripe.Internal.StripeError
import           Web.Stripe.Util

config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

-- Create card
newtype CardId = CardId Text deriving (Show, Eq)
newtype TokenId = TokenId Text deriving (Show, Eq, Ord)
newtype CustomerId = CustomerId Text deriving (Show, Eq, Ord)

createCardByTokenId :: FromJSON a => CustomerId -> TokenId -> IO (Either StripeError a)
createCardByTokenId (CustomerId cid) (TokenId tokenId) = sendStripeRequest config req []
  where req = StripeRequest POST url
        url = "customers/" <> cid
        params = [("card", T.encodeUtf8 tokenId)] -- card is required

createCard :: FromJSON a => CustomerId -> TokenId -> IO (Either StripeError a)
createCard (CustomerId cid) (TokenId tokenId) = sendStripeRequest config req []
  where req = StripeRequest POST url
        url = "customers/" <> cid
        params = [("card", T.encodeUtf8 tokenId)] -- card is required

data Brand = Visa | AMEX | MasterCard
           | Discover | JCB | DinersClub | Unknown deriving (Show, Eq)

instance FromJSON Brand where
   parseJSON (Object o) = 
       do typ <- o .: "brand"
          return $ case typ of
            "Visa" -> Visa
            "American Express" -> AMEX
            "MasterCard" -> MasterCard
            "Discover" -> Discover
            "JCB" -> JCB
            "DinersClub" -> DinersClub
            otherwise -> Unknown
                          

data Card = Card {
      cardId                  :: Text
    , cardLastFour            :: Text
    , cardBrand               :: Brand
    , cardFunding             :: Text
    , cardExpMonth            :: Int
    , cardExpYear             :: Int
    , cardFingerprint         :: Text
    , cardCountry             :: Text
    , cardName                :: Maybe Text
    , cardAddress_line1       :: Maybe Text
    , cardAddress_line2       :: Maybe Text
    , cardAddress_city        :: Maybe Text
    , cardAddress_state       :: Maybe Text
    , cardAddress_zip         :: Maybe Text
    , cardAddress_country     :: Maybe Text
    , cardCvc_check           :: Maybe Text
    , cardAddress_line1_check :: Maybe Text
    , cardAddress_zip_check   :: Maybe Text
    , cardCustomer            :: Maybe Text
} deriving (Show, Eq)



instance FromJSON Card where
    parseJSON (Object o) =
        Card <$> o .: "id"
             <*> o .: "last4"
             <*> o .: "brand"
             <*> o .: "funding"
             <*> o .: "exp_month"
             <*> o .: "exp_year"
             <*> o .: "fingerprint"
             <*> o .: "country"
             <*> o .:? "name"
             <*> o .:? "address_line1"
             <*> o .:? "address_line2"
             <*> o .:? "address_city"
             <*> o .:? "address_state"
             <*> o .:? "address_zip"
             <*> o .:? "address_country"
             <*> o .:? "cvc_check"
             <*> o .:? "address_line1_check"
             <*> o .:? "address_zip_check"
             <*> o .:? "customer"


newtype CardNumber = CardNumber Int deriving (Show, Eq, Ord)
newtype CardExpMonth = CardExpMonth Int deriving (Show, Eq, Ord)
newtype CardExpYear = CardExpYear Int deriving (Show, Eq, Ord)
newtype CardCVC = CardCVC Int deriving (Show, Eq, Ord)

createCardToken :: CardNumber -> CardExpMonth -> CardExpYear -> CardCVC -> 
                   IO (Either StripeError Token)
createCardToken (CardNumber num) (CardExpMonth month) (CardExpYear year) (CardCVC cvc)
    = sendStripeRequest config req params
  where req = StripeRequest POST "tokens"
        params = [ ("card[number]", toBS num)
                 , ("card[exp_month]", toBS month)
                 , ("card[exp_year]", toBS year)
                 , ("card[cvc]", toBS cvc)
                 ]

data Token = Token {
      tokenId       :: Text
    , tokenLiveMode :: Bool
    , tokenCreated  :: UTCTime
    , tokenUsed     :: Bool
    , tokenType     :: Text
    , tokenCard     :: Card
} deriving (Show, Eq)

instance FromJSON Token where
   parseJSON (Object o) =
       Token <$> o .: "id"
             <*> o .: "livemode"
             <*> (fromSeconds <$> o .: "created")
             <*> o .: "used"
             <*> o .: "type"
             <*> o .: "card"


-- See all the optional arguments here, lots of them
-- updateCard :: CustomerId -> Card -> IO ()
-- updateCard (CustomerId custId) (Card cardId) = sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "customers/" <> custId <> "/cards/" <> cardId

-- deleteCard :: CustomerId -> Card -> IO ()
-- deleteCard (CustomerId custId) (Card cardId) = sendStripeRequest req config
--   where req = StripeRequest DELETE url []
--         url = "customers/" <> custId <> "/cards/" <> cardId

-- -- optional args
-- -- ending_before
-- -- limit
-- -- starting_after

-- getCards :: CustomerId -> IO ()
-- getCards (CustomerId custId) = sendStripeRequest req config
--   where req = StripeRequest GET url []
--         url = "customers/" <> custId <> "/cards"



