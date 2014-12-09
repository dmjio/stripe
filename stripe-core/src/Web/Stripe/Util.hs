{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.Util
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Util
    ( -- * Utils
      fromSeconds
    , toSeconds
    , paramsToByteString
    , toText
    , toTextLower
    , getParams
    , toBytestring
    , (</>)
    , toMetaData
    , toExpandable
    ) where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as B8
import           Data.Monoid           (Monoid, mconcat, mempty, (<>))
import           Data.String           (IsString)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)

------------------------------------------------------------------------------
-- | Conversion from a `Show` constrained type to `Text`
toText
    :: Show a
    => a
    -> Text
toText = T.pack . show

------------------------------------------------------------------------------
-- | Conversion from a `Show` constrained type to lowercase `Text`
toTextLower
    :: Show a
    => a
    -> Text
toTextLower = T.toLower . T.pack . show

------------------------------------------------------------------------------
-- | Conversion of a key value pair to a query parameterized string
paramsToByteString
    :: (Monoid m, IsString m)
    => [(m, m)]
    -> m
paramsToByteString []           = mempty
paramsToByteString ((x,y) : []) = x <> "=" <> y
paramsToByteString ((x,y) : xs) =
    mconcat [ x, "=", y, "&" ] <> paramsToByteString xs

------------------------------------------------------------------------------
-- | Forward slash interspersion on `Monoid` and `IsString`
-- constrained types
(</>)
    :: (Monoid m, IsString m)
    => m
    -> m
    -> m
m1 </> m2 = m1 <> "/" <> m2

------------------------------------------------------------------------------
-- | Convert an `Integer` to a `UTCTime`
fromSeconds
    :: Integer
    -> UTCTime
fromSeconds = posixSecondsToUTCTime . fromInteger

------------------------------------------------------------------------------
-- | Convert a `UTCTime` to a `Integer`
toSeconds
    :: UTCTime
    -> Integer
toSeconds = round . utcTimeToPOSIXSeconds

------------------------------------------------------------------------------
-- | Retrieve and encode the optional parameters
getParams
    :: [(ByteString, Maybe Text)]
    -> [(ByteString, ByteString)]
getParams xs = [ (x, T.encodeUtf8 y) | (x, Just y) <- xs ]

------------------------------------------------------------------------------
-- | Convert APITVersion to a ByteString
toBytestring :: Show a => a -> ByteString
toBytestring = B8.pack . show

------------------------------------------------------------------------------
-- | To MetaData
toMetaData :: [(Text, Text)] -> [(ByteString, ByteString)]
toMetaData = map toKV
  where
    toKV (k,v) = ("metadata[" <> T.encodeUtf8 k <> "]",  T.encodeUtf8 v)

------------------------------------------------------------------------------
-- | To Expandable
toExpandable :: [Text] -> [(ByteString, ByteString)]
toExpandable = map toKV
  where
    toKV v = ("expand[]",  T.encodeUtf8 v)

