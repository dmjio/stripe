{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Client.Types
  ( -- * Types
    Stripe
  , StripeRequest (..)
  , StripeConfig  (..)
  ) where

import           Control.Monad.Trans.Reader (ReaderT)
import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Network.Http.Client        (Method)
import           Web.Stripe.Client.Error    (StripeError)

------------------------------------------------------------------------------
-- | Base Type we use for Stripe
type Stripe a = ReaderT StripeConfig IO (Either StripeError a)

------------------------------------------------------------------------------
-- | HTTP Params type
type Params = [(ByteString, ByteString)]

------------------------------------------------------------------------------
-- | Stripe Request holding `Method`, URL and `Params` for a Request
data StripeRequest = StripeRequest
    { method :: Method
    , url    :: Text
    , params :: Params
    } deriving Show

------------------------------------------------------------------------------
-- | Information for Stripe secret key and API Version
data StripeConfig = StripeConfig
    { secretKey  :: ByteString
    , apiVersion :: ByteString
    } deriving Show
