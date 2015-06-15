{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Transfer
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#transfers >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Transfer
-- import Web.Stripe.Recipient
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $ getRecipient (RecipientId "recipient_id")
--   case result of
--     (Left stripeError) -> print stripeError
--     (Right (Recipient { recipientId = recipientid })) -> do
--       result <- stripe config $ createTransfer recipientid (Amount 100) USD
--       case result of
--        Left  stripeError -> print stripeError
--        Right transfer    -> print transfer
-- @
module Web.Stripe.Transfer
    ( -- * API
      CreateTransfer
    , createTransfer
    , GetTransfer
    , getTransfer
    , UpdateTransfer
    , updateTransfer
    , CancelTransfer
    , cancelTransfer
    , GetTransfers
    , getTransfers
      -- * Types
    , Amount          (..)
    , BankAccountId   (..)
    , Card            (..)
    , CardId          (..)
    , Created         (..)
    , Currency        (..)
    , Date            (..)
    , Description     (..)
    , EndingBefore    (..)
    , ExpandParams    (..)
    , Recipient       (..)
    , RecipientId     (..)
    , StartingAfter   (..)
    , StatementDescription (..)
    , StripeList      (..)
    , Transfer        (..)
    , TransferId      (..)
    , TransferStatus  (..)
    , TransferType    (..)
    , Limit           (..)
    ) where
import           Web.Stripe.StripeRequest (Method (GET, POST),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (Amount(..), BankAccountId(..), Card(..),
                                           CardId(..), Created(..),Currency (..),
                                           Date(..), EndingBefore(..),
                                           ExpandParams(..),
                                           Limit(..), MetaData(..), Recipient (..),
                                           RecipientId(..), StartingAfter(..),
                                           StatementDescription(..),
                                           StripeList (..), Transfer (..),
                                           TransferId (..), TransferStatus (..),
                                           Description(..), TransferType (..))

------------------------------------------------------------------------------
-- | Create a `Transfer`
createTransfer
    :: RecipientId -- ^ The `RecipientId` of the `Recipient` who will receive the `Transfer`
    -> Amount      -- ^ The `Amount` of money to transfer to the `Recipient`
    -> Currency    -- ^ The `Currency` in which to perform the `Transfer`
    -> StripeRequest CreateTransfer
createTransfer
    recipientid
    amount
    currency    = request
  where request = mkStripeRequest POST url params
        url     = "transfers"
        params  = toStripeParam recipientid $
                  toStripeParam amount      $
                  toStripeParam currency    $
                  []

data CreateTransfer
type instance StripeReturn CreateTransfer = Transfer
instance StripeHasParam CreateTransfer Description
instance StripeHasParam CreateTransfer BankAccountId
instance StripeHasParam CreateTransfer CardId
instance StripeHasParam CreateTransfer StatementDescription
instance StripeHasParam CreateTransfer MetaData

------------------------------------------------------------------------------
-- | Retrieve a `Transfer`
getTransfer
    :: TransferId -- ^ `TransferId` associated with the `Transfer` to retrieve
    -> StripeRequest GetTransfer
getTransfer
    (TransferId transferid)
                = request
  where request = mkStripeRequest GET url params
        url     = "transfers" </> transferid
        params  = []

data GetTransfer
type instance StripeReturn GetTransfer = Transfer
instance StripeHasParam GetTransfer ExpandParams

------------------------------------------------------------------------------
-- | Update a `Transfer`
updateTransfer
    :: TransferId        -- ^ The `TransferId` of the `Transfer` to update
    -> StripeRequest UpdateTransfer
updateTransfer
    (TransferId transferid)
                = request
  where request = mkStripeRequest POST url params
        url     = "transfers" </> transferid
        params  = []

data UpdateTransfer
type instance StripeReturn UpdateTransfer = Transfer
instance StripeHasParam UpdateTransfer Description
instance StripeHasParam UpdateTransfer MetaData

------------------------------------------------------------------------------
-- | Cancel a `Transfer`
cancelTransfer
    :: TransferId        -- ^ The `TransferId` of the `Transfer` to cancel
    -> StripeRequest CancelTransfer
cancelTransfer (TransferId transferid) = request
  where request = mkStripeRequest POST url params
        url     = "transfers" </> transferid </> "cancel"
        params  = []

data CancelTransfer
type instance StripeReturn CancelTransfer = Transfer

------------------------------------------------------------------------------
-- | Retrieve StripeList of `Transfers`
getTransfers
    :: StripeRequest GetTransfers
getTransfers
    = request
  where request = mkStripeRequest GET url params
        url     = "transfers"
        params  = []

data GetTransfers
type instance StripeReturn GetTransfers = StripeList Transfer
instance StripeHasParam GetTransfers ExpandParams
instance StripeHasParam GetTransfers Created
instance StripeHasParam GetTransfers Date
instance StripeHasParam GetTransfers (EndingBefore TransferId)
instance StripeHasParam GetTransfers Limit
instance StripeHasParam GetTransfers RecipientId
instance StripeHasParam GetTransfers (StartingAfter TransferId)
instance StripeHasParam GetTransfers TransferStatus
