module Web.Stripe.Internal.Class
    ( URLDecodeable (..)
    , ByteString
    ) where

import Data.ByteString (ByteString)

class URLDecodeable a where
    formEncode :: a -> [(ByteString, ByteString)]


