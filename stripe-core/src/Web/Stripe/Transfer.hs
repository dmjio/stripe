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
-- import Web.Stripe
-- import Web.Stripe.Transfer
-- import Web.Stripe.Recipient
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $ do
--     Recipient { recipientId = recipientid } <- getRecipient (RecipientId "recipient_id")
--     createTransfer recipientid (100 :: Amount) USD ([] :: MetaData)
--   case result of
--     Right transfer    -> print transfer
--     Left  stripeError -> print stripeError
-- @
module Web.Stripe.Transfer
    ( -- * API
      CreateTransfer
    , createTransfer
    , GetTransfer
    , getTransfer
    , getTransferExpandable
    , UpdateTransfer
    , updateTransfer
    , CancelTransfer
    , cancelTransfer
    , GetTransfers
    , getTransfers
    , getTransfersExpandable
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
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          (toExpandable, (</>))
import           Web.Stripe.Types         (Amount(..), BankAccountId(..), Card(..),
                                           CardId(..), Created(..),Currency (..),
                                           Date(..), EndingBefore(..), ExpandParams,
                                           Limit(..), MetaData(..), Recipient (..),
                                           RecipientId(..), StartingAfter(..),
                                           StatementDescription(..),
                                           StripeList (..), Transfer (..),
                                           TransferId (..), TransferStatus (..),
                                           Description(..), TransferType (..))
import           Web.Stripe.Types.Util    (getRecipientId)

------------------------------------------------------------------------------
-- | Create a `Transfer`
data CreateTransfer
type instance StripeReturn CreateTransfer = Transfer
instance StripeHasParam Transfer Description
instance StripeHasParam Transfer BankAccountId
-- instance StripeHasParam Transfer Card -- FIXME
instance StripeHasParam Transfer CardId
instance StripeHasParam Transfer StatementDescription
instance StripeHasParam Transfer MetaData
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

------------------------------------------------------------------------------
-- | Retrieve a `Transfer`
data GetTransfer
type instance StripeReturn GetTransfer = Transfer
getTransfer
    :: TransferId -- ^ `TransferId` associated with the `Transfer` to retrieve
    -> StripeRequest GetTransfer
getTransfer transferid =
    getTransferExpandable transferid []

------------------------------------------------------------------------------
-- | Retrieve a `Transfer` with `ExpandParams`
getTransferExpandable
    :: TransferId   -- ^ `TransferId` associated with the `Transfer` to retrieve
    -> ExpandParams -- ^ The `ExpandParams` of the object to be expanded
    -> StripeRequest GetTransfer
getTransferExpandable
    (TransferId transferid)
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "transfers" </> transferid
        params  = toExpandable expandParams
------------------------------------------------------------------------------
-- | Update a `Transfer`
data UpdateTransfer
type instance StripeReturn UpdateTransfer = Transfer
instance StripeHasParam UpdateTransfer Description
instance StripeHasParam UpdateTransfer MetaData
updateTransfer
    :: TransferId        -- ^ The `TransferId` of the `Transfer` to update
    -> StripeRequest UpdateTransfer
updateTransfer
    (TransferId transferid)
                = request
  where request = mkStripeRequest POST url params
        url     = "transfers" </> transferid
        params  = []

------------------------------------------------------------------------------
-- | Cancel a `Transfer`
data CancelTransfer
type instance StripeReturn CancelTransfer = Transfer
cancelTransfer
    :: TransferId        -- ^ The `TransferId` of the `Transfer` to cancel
    -> StripeRequest CancelTransfer
cancelTransfer (TransferId transferid) = request
  where request = mkStripeRequest POST url params
        url     = "transfers" </> transferid </> "cancel"
        params  = []


------------------------------------------------------------------------------
-- | Retrieve StripeList of `Transfers`
data GetTransfers
type instance StripeReturn GetTransfers = StripeList Transfer
instance StripeHasParam GetTransfers Created
instance StripeHasParam GetTransfers Date
instance StripeHasParam GetTransfers (EndingBefore TransferId)
instance StripeHasParam GetTransfers Limit
instance StripeHasParam GetTransfers RecipientId
instance StripeHasParam GetTransfers (StartingAfter TransferId)
instance StripeHasParam GetTransfers TransferStatus
getTransfers
    :: StripeRequest GetTransfers
getTransfers
    = getTransfersExpandable []

------------------------------------------------------------------------------
-- | Retrieve StripeList of `Transfers` with `ExpandParams`
getTransfersExpandable
    :: ExpandParams             -- ^ The `ExpandParams` of the object to be expanded
    -> StripeRequest GetTransfers
getTransfersExpandable
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "transfers"
        params  = toExpandable expandParams
