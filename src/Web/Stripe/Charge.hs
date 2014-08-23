{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Stripe.Charge 
    ( Charge(..)
    ) where


import           Control.Applicative
import           Data.Aeson
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text.Encoding              as T
import           Network.Http.Client
import           Web.Stripe.Client.Internal
import           Web.Stripe.Internal.StripeError
import           Web.Stripe.Util
import           Data.Time

config = StripeConfig "sk_test_zvqdM2SSA6WwySqM6KJQrqpH" "2014-03-28"

charge :: Amount -> Currency -> Stripe Charge
charge (Amount amount) (Currency currency) = callAPI request
  where request = StripeRequest POST url params
        url     = "charges"
        params  = [ ("amount", toBs amount)
                  , ("currency", toBs currency)
                  ]
       

-- chargeByCardId :: Card -> IO ()
-- chargeByCardId card = charge $ Left card

-- chargeByCustomerId :: CustomerId -> IO ()
-- chargeByCustomerId customerId = charge $ Right customerId

-- getCharge :: FromJSON a => Charge -> IO (Either StripeError a)
-- getCharge (Charge charge) = sendStripeRequest config req ()
--   where req = StripeRequest GET url 
--         url = "charges/" <> charge



-- -- description + metadata == options
-- updateCharge :: Charge -> IO ()
-- updateCharge (Charge charge) = sendStripeRequest req config
--   where req = StripeRequest POST url []
--         url = "charges/" <> charge

-- -- optional params


-- captureCharge :: FromJSON a => Charge -> IO (Either StripeError a)
-- captureCharge (Charge charge) = sendStripeRequest config req ()
--   where req = StripeRequest POST url
--         url = "charges/" <> charge <> "/capture"

-- -- optional params
-- getCharges :: FromJSON a => IO (Either StripeError a)
-- getCharges = sendStripeRequest config req chargeOptions
--   where req = StripeRequest GET "charges" []
-- --


