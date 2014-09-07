{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Client.Util
    ( fromSeconds
    , paramsToByteString
    , toText
    , getParams
    , (</>)
    ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as S
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy  as BL
import           Data.Monoid
import           Data.String
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)

toText :: Show a => a -> Text
toText= T.pack . show

paramsToByteString :: [(ByteString, ByteString)] -> ByteString
paramsToByteString [] = ""
paramsToByteString ((x,y) : []) = x <> "=" <> y
paramsToByteString ((x,y) : xs) = x <> "=" <> y <> "&" <> paramsToByteString xs

(</>) :: (Monoid m, IsString m) => m -> m -> m
m1 </> m2 = m1 <> "/" <> m2

fromSeconds :: Integer -> UTCTime
fromSeconds  = posixSecondsToUTCTime . fromInteger

getParams :: [(ByteString, Maybe Text)] -> [(ByteString, ByteString)]
getParams xs = [ (x, T.encodeUtf8 y) | (x, Just y) <- xs ]
