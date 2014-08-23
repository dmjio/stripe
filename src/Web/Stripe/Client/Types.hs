{-# LANGUAGE OverloadedStrings #-}

module Web.Stripe.Client.Types 
  ( Stripe 
  , Params
  , StripeRequest (..)
  , StripeConfig (..)
  , StripeDeleteResult (..)
  ) where

import           Control.Applicative
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Reader            (ReaderT, ask, runReaderT)
import           Data.Aeson
import           Data.ByteString                 (ByteString)
import           Data.Text                       (Text)
import           Network.Http.Client
import           Web.Stripe.Client.Error

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
