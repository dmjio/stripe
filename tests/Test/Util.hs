module Test.Util
       ( -- * Helpers
         makePlanId
       , makeCouponId
       ) where

import           System.Random
import           Control.Applicative
import qualified Data.Text as T
import           Data.Text    (Text)
import           Control.Monad

import           Web.Stripe.Plan
import           Web.Stripe.Coupon

------------------------------------------------------------------------------
-- | `PlanId` creation helper
makePlanId :: IO PlanId
makePlanId = PlanId <$> makeGuid

------------------------------------------------------------------------------
-- | `CouponId` creation helper
makeCouponId :: IO CouponId
makeCouponId = CouponId <$> makeGuid

------------------------------------------------------------------------------
-- | Guid Creation Helper
makeGuid :: IO Text
makeGuid = T.pack <$> replicateM 10 (randomRIO ('a', 'z'))

