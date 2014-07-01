module Web.Stripe.Util
    ( fromSeconds
    ) where

import           Data.Time.Clock 
import           Data.Time.Clock.POSIX  (posixSecondsToUTCTime,
                                         utcTimeToPOSIXSeconds)



fromSeconds :: Integer -> UTCTime
fromSeconds  = posixSecondsToUTCTime . fromInteger
