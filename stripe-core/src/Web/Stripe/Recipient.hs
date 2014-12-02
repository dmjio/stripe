{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Recipient
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#recipients >
--
-- @
-- import Web.Stripe
-- import Web.Stripe.Recipient
--
-- main :: IO ()
-- main = do
--   let config = SecretKey "secret_key"
--   result <- stripe config $
--       createRecipient (FirstName "simon")
--                       (LastName "marlow")
--                       Nothing -- What is Simon Marlow's middle initial?
--                       (Invidiual :: RecipientType)
--   case result of
--     Right recipient  -> print recipient
--     Left stripeError -> print stripeError
-- @
module Web.Stripe.Recipient
    ( -- * API
      CreateRecipient
    , createRecipient
    , GetRecipient
    , getRecipient
    , getRecipientExpandable
    , UpdateRecipient
    , updateRecipient
    , DeleteRecipient
    , deleteRecipient
    , GetRecipients
    , getRecipients
    , getRecipientsExpandable
      -- * Types
    , CVC            (..)
    , Description    (..)
    , Recipient      (..)
    , RecipientId    (..)
    , Name           (..)
    , RecipientType  (..)
    , TaxID          (..)
    , BankAccount    (..)
    , TokenId        (..)
    , CardNumber     (..)
    , ExpMonth       (..)
    , ExpYear        (..)
    , Email          (..)
    , Limit          (..)
    , StripeDeleteResult (..)
    , RoutingNumber  (..)
    , AccountNumber  (..)
    , Country        (..)
    , AddressCity    (..)
    , AddressCountry (..)
    , AddressLine1   (..)
    , AddressLine2   (..)
    , AddressState   (..)
    , AddressZip     (..)
    , BankAccountId  (..)
    , BankAccountStatus (..)
    , IsVerified     (..)
    ) where

import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          (toExpandable, (</>))
import           Web.Stripe.Types         (AccountNumber (..),
                                           BankAccount (..), CVC (..),
                                           CardId (..), CardNumber, BankAccountId (..),
                                           BankAccountStatus(..),
                                           CardNumber (..), Country (..),
                                           Description(..), Email, Email (..),
                                           ExpMonth (..), ExpYear(..), IsVerified(..),
                                           RoutingNumber  (..), AccountNumber  (..),
                                           Country        (..), AddressCity    (..),
                                           AddressCountry (..), AddressLine1   (..),
                                           AddressLine2   (..), AddressState   (..),
                                           AddressZip     (..), ExpYear (..),
                                           Limit(..), Name(..), Recipient (..),
                                           RecipientId (..), ExpandParams,
                                           RecipientType (..), StripeDeleteResult(..),
                                           RoutingNumber (..), EndingBefore(..),
                                           StartingAfter(..), StripeList (..),
                                           TaxID(..), TokenId(..),
                                           TokenId (..), MetaData(..))
import           Web.Stripe.Types.Util    (getRecipientId)

------------------------------------------------------------------------------
-- | Base Request for issues create `Recipient` requests
data CreateRecipient
type instance StripeReturn CreateRecipient = Recipient
instance StripeHasParam CreateRecipient TaxID
-- instance StripeHasParam CreateRecipient BankAccount -- FIXME
instance StripeHasParam CreateRecipient TokenId
-- instance StripeHasParam CreateRecipient Card -- FIXME
instance StripeHasParam CreateRecipient CardId
instance StripeHasParam CreateRecipient Email
instance StripeHasParam CreateRecipient Description
instance StripeHasParam CreateRecipient MetaData
createRecipient
    :: Name
    -> RecipientType
    -> StripeRequest CreateRecipient
createRecipient
    name
    recipienttype
                = request
  where request = mkStripeRequest POST url params
        url     = "recipients"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve a 'Recipient'
data GetRecipient
type instance StripeReturn GetRecipient = Recipient
getRecipient
    :: RecipientId -- ^ The `RecipientId` of the `Recipient` to be retrieved
    -> StripeRequest GetRecipient
getRecipient
    recipientid = getRecipientExpandable recipientid []

------------------------------------------------------------------------------
-- | Retrieve a `Recipient`
getRecipientExpandable
    :: RecipientId   -- ^ The `RecipientId` of the `Recipient` to be retrieved
    -> ExpandParams  -- ^ `ExpandParams` of the object to be expanded
    -> StripeRequest GetRecipient
getRecipientExpandable
    recipientid
    expandParams = request
  where request = mkStripeRequest GET url params
        url     = "recipients" </> getRecipientId recipientid
        params  = toExpandable expandParams

------------------------------------------------------------------------------
-- | Update `Recipient`
data UpdateRecipient
type instance StripeReturn UpdateRecipient = Recipient
instance StripeHasParam UpdateRecipient Name
instance StripeHasParam UpdateRecipient TaxID
-- instance StripeHasParam UpdateRecipient BankAccount -- FIXME
instance StripeHasParam UpdateRecipient TokenId
-- instance StripeHasParam UpdateRecipient Card        -- FIXME
-- instance StripeHasParam UpdateRecipient DefaultCard -- FIXME
instance StripeHasParam UpdateRecipient CardId
instance StripeHasParam UpdateRecipient Email
instance StripeHasParam UpdateRecipient Description
instance StripeHasParam UpdateRecipient MetaData
updateRecipient
    :: RecipientId
    -> StripeRequest UpdateRecipient
updateRecipient
  recipientid   = request
  where request = mkStripeRequest POST url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []

------------------------------------------------------------------------------
-- | Delete a `Recipient`
data DeleteRecipient
type instance StripeReturn DeleteRecipient = StripeDeleteResult
deleteRecipient
    :: RecipientId   -- ^ `RecipiendId` of `Recipient` to delete
    -> StripeRequest DeleteRecipient
deleteRecipient
   recipientid = request
  where request = mkStripeRequest DELETE url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []

------------------------------------------------------------------------------
-- | Retrieve multiple 'Recipient's
data GetRecipients
type instance StripeReturn GetRecipients = StripeList Recipient
instance StripeHasParam GetRecipients (EndingBefore RecipientId)
instance StripeHasParam GetRecipients Limit
instance StripeHasParam GetRecipients (StartingAfter RecipientId)
instance StripeHasParam GetRecipients IsVerified
getRecipients
    :: StripeRequest GetRecipients
getRecipients
  = getRecipientsExpandable []

------------------------------------------------------------------------------
-- | Retrieve multiple 'Recipient's with `ExpandParams`
getRecipientsExpandable
    :: ExpandParams              -- ^ `ExpandParams` of the object to be expanded
    -> StripeRequest GetRecipients
getRecipientsExpandable
  expandParams  = request
  where request = mkStripeRequest GET url params
        url     = "recipients"
        params  = toExpandable expandParams
