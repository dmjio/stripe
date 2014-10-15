{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Client.Types
  ( -- * Types
    Stripe
  , StripeRequest (..)
  , StripeConfig  (..)
  , APIVersion    (..)
  ) where

import           Control.Monad.Reader       (ReaderT)
import           Control.Monad.Trans.Either (EitherT)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Network.Http.Client        (Connection, Method)
import           Web.Stripe.Client.Error    (StripeError (..))

------------------------------------------------------------------------------
-- | Base Type we use for Stripe
type Stripe a = EitherT StripeError (ReaderT (StripeConfig, Connection) IO) a
-- type Stripe a = ReaderT StripeConfig IO (Either StripeError a)

------------------------------------------------------------------------------
-- | HTTP Params type
type Params = [(ByteString, ByteString)]

------------------------------------------------------------------------------
-- | Stripe Request holding `Method`, URL and `Params` for a Request
data StripeRequest = StripeRequest
    { method      :: Method -- ^ Method of StripeRequest (i.e. `GET`, `PUT`, `POST`, `PUT`)
    , endpoint    :: Text   -- ^ Endpoint of StripeRequest
    , queryParams :: Params -- ^ Query Parameters of StripeRequest
    } deriving Show

------------------------------------------------------------------------------
-- | Information for Stripe secret key
data StripeConfig = StripeConfig
    { secretKey :: ByteString
    } deriving Show

data APIVersion =
    V20141007 -- ^ Stripe API Version for this package release
    deriving Eq

instance Show APIVersion where
    show V20141007 = "2014-10-07"

