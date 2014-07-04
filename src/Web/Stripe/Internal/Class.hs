module Web.Stripe.Internal.Class
    ( URLEncodeable (..)
    , ByteString
    ) where

import Data.ByteString (ByteString)

class URLEncodeable a where
    formEncode :: a -> [(ByteString, ByteString)]


