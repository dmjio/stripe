{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Util
    ( fromSeconds
    , convertToString
    , strictToLazy
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

strictToLazy :: S.ByteString -> BL.ByteString
strictToLazy = BL.fromChunks . (:[])

convertToString :: [(S.ByteString, S.ByteString)] -> S.ByteString
convertToString [] = ""
convertToString ((x,y) : []) = x <> "=" <> y
convertToString ((x,y) : xs) = x <> "=" <> y <> "&" <> convertToString xs

fromSeconds :: Integer -> UTCTime
fromSeconds  = posixSecondsToUTCTime . fromInteger
