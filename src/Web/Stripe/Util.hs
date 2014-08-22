{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Util
    ( fromSeconds
    , paramsToByteString
    , toBS
    ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy  as BL
import           Data.Monoid
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)

toBS :: Show a => a -> S.ByteString
toBS = BC8.pack . show

paramsToByteString :: [(S.ByteString, S.ByteString)] -> S.ByteString
paramsToByteString [] = ""
paramsToByteString ((x,y) : []) = x <> "=" <> y
paramsToByteString ((x,y) : xs) = x <> "=" <> y <> "&" <> paramsToByteString xs

fromSeconds :: Integer -> UTCTime
fromSeconds  = posixSecondsToUTCTime . fromInteger
