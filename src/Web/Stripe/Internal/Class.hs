module Web.Stripe.Internal.Class
    ( URLDecodeable
    ) where

class URLDecodeable a where
    formEncode :: a -> [(ByteString, ByteString)]


