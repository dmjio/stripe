{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Web.Stripe.Client.Util
-- Description : utilities for building Stripe requests
-- Copyright   : (c) David Johnson, 2014
-- License     : BSD3
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX

module Web.Stripe.Client.Util
    ( fromSeconds
    , paramsToByteString
    , toText
    , getParams
    , (</>)
    ) where

import           Data.ByteString       (ByteString)
import           Data.Monoid           (Monoid, (<>), mempty, mconcat)
import           Data.String           (IsString)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as T
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)
toText 
    :: Show a 
    => a
    -> Text
toText = T.pack . show

paramsToByteString
    :: (Monoid m, IsString m)
    => [(m, m)]
    -> m
paramsToByteString []           = mempty
paramsToByteString ((x,y) : []) = x <> "=" <> y
paramsToByteString ((x,y) : xs) = 
    mconcat [ x, "=", y, "&" ] <> paramsToByteString xs

(</>) 
    :: (Monoid m, IsString m)
    => m
    -> m
    -> m
m1 </> m2 = m1 <> "/" <> m2

fromSeconds
    :: Integer
    -> UTCTime
fromSeconds = posixSecondsToUTCTime . fromInteger

getParams
    :: [(ByteString, Maybe Text)]
    -> [(ByteString, ByteString)]
getParams xs = [ (x, T.encodeUtf8 y) | (x, Just y) <- xs ]
