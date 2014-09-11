{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Client.Types
  ( Stripe
  , StripeRequest (..)
  , StripeConfig (..)
  , StripeDeleteResult (..)
  ) where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (mzero)
import           Control.Monad.Reader    (ReaderT)
import           Data.Aeson              (FromJSON(parseJSON), (.:), Value(Object))
import           Data.ByteString         (ByteString)
import           Data.Text               (Text)
import           Network.Http.Client     (Method)
import           Web.Stripe.Client.Error (StripeError)

-- Base Type we use for Stripe
type Stripe a = ReaderT StripeConfig IO (Either StripeError a)

-- HTTP Params type
type Params = [(ByteString, ByteString)]

data StripeRequest = StripeRequest
    { method :: Method
    , url    :: Text
    , params :: Params
    } deriving (Show)

data StripeConfig = StripeConfig
    { secretKey  :: ByteString
    , apiVersion :: ByteString
    } deriving (Show)

data StripeDeleteResult = StripeDeleteResult {
      deleted   :: Bool
    , deletedId :: Text
    } deriving (Show, Eq)

instance FromJSON StripeDeleteResult where
   parseJSON (Object o) =
       StripeDeleteResult <$> o .: "deleted"
                          <*> o .: "id"
   parseJSON _ = mzero
