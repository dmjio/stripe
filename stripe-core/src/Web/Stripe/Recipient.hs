{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Recipient
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : code@dmj.io
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#recipients >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Recipient
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $
--       createRecipient (Name "simon marlow")
--                       Individual
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
    , UpdateRecipient
    , updateRecipient
    , DeleteRecipient
    , deleteRecipient
    , GetRecipients
    , getRecipients
      -- * Types
    , AccountNumber  (..)
    , AddressCity    (..)
    , AddressCountry (..)
    , AddressLine1   (..)
    , AddressLine2   (..)
    , AddressState   (..)
    , AddressZip     (..)
    , BankAccount    (..)
    , BankAccountId  (..)
    , BankAccountStatus (..)
    , CardNumber     (..)
    , Country        (..)
    , CVC            (..)
    , DefaultCard    (..)
    , Description    (..)
    , ExpandParams   (..)
    , ExpMonth       (..)
    , ExpYear        (..)
    , Email          (..)
    , IsVerified     (..)
    , Limit          (..)
    , MetaData       (..)
    , Name           (..)
    , NewBankAccount (..)
    , NewCard        (..)
    , Recipient      (..)
    , RecipientId    (..)
    , RecipientType  (..)
    , RoutingNumber  (..)
    , StripeDeleteResult (..)
    , TaxID          (..)
    , TokenId        (..)
    ) where

import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE),
                                           StripeHasParam, StripeRequest (..),
                                           ToStripeParam(..), StripeReturn,
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (AccountNumber (..),
                                           BankAccount (..), CVC (..),
                                           CardId (..), CardNumber, BankAccountId (..),
                                           BankAccountStatus(..),
                                           CardNumber (..), Country (..),
                                           DefaultCard(..), Description(..), Email(..),
                                           ExpMonth (..), ExpYear(..), IsVerified(..),
                                           RoutingNumber  (..), AccountNumber  (..),
                                           Country        (..), AddressCity    (..),
                                           AddressCountry (..), AddressLine1   (..),
                                           AddressLine2   (..), AddressState   (..),
                                           AddressZip     (..), Country(..), ExpYear (..),
                                           Limit(..), Name(..), NewBankAccount(..), NewCard(..), Recipient (..),
                                           RecipientId (..), ExpandParams(..),
                                           RecipientType (..), StripeDeleteResult(..),
                                           RoutingNumber (..), EndingBefore(..),
                                           StartingAfter(..), StripeList (..),
                                           TaxID(..), TokenId(..),
                                           TokenId (..), MetaData(..))
import           Web.Stripe.Types.Util    (getRecipientId)

------------------------------------------------------------------------------
-- | Base Request for issues create `Recipient` requests
createRecipient
    :: Name          -- ^ `Recipient` `Name`
    -> RecipientType -- ^ `Individual` or `Corporation`
    -> StripeRequest CreateRecipient
createRecipient
    name
    recipienttype
                = request
  where request = mkStripeRequest POST url params
        url     = "recipients"
        params  = toStripeParam name $
                  toStripeParam recipienttype $
                  []

data CreateRecipient
type instance StripeReturn CreateRecipient = Recipient
instance StripeHasParam CreateRecipient TaxID
instance StripeHasParam CreateRecipient NewBankAccount
instance StripeHasParam CreateRecipient TokenId
instance StripeHasParam CreateRecipient NewCard
instance StripeHasParam CreateRecipient CardId
instance StripeHasParam CreateRecipient Email
instance StripeHasParam CreateRecipient Description
instance StripeHasParam CreateRecipient MetaData

------------------------------------------------------------------------------
-- | Retrieve a 'Recipient'
getRecipient
    :: RecipientId -- ^ The `RecipientId` of the `Recipient` to be retrieved
    -> StripeRequest GetRecipient
getRecipient
  recipientid   = request
  where request = mkStripeRequest GET url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []

data GetRecipient
type instance StripeReturn GetRecipient = Recipient
instance StripeHasParam GetRecipient ExpandParams

------------------------------------------------------------------------------
-- | Update `Recipient`
updateRecipient
    :: RecipientId -- ^ `RecipientId` of `Recipient` to update
    -> StripeRequest UpdateRecipient
updateRecipient
  recipientid   = request
  where request = mkStripeRequest POST url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []

data UpdateRecipient
type instance StripeReturn UpdateRecipient = Recipient
instance StripeHasParam UpdateRecipient Name
instance StripeHasParam UpdateRecipient TaxID
instance StripeHasParam UpdateRecipient NewBankAccount
instance StripeHasParam UpdateRecipient TokenId
instance StripeHasParam UpdateRecipient NewCard
instance StripeHasParam UpdateRecipient DefaultCard
instance StripeHasParam UpdateRecipient CardId
instance StripeHasParam UpdateRecipient Email
instance StripeHasParam UpdateRecipient Description
instance StripeHasParam UpdateRecipient MetaData

------------------------------------------------------------------------------
-- | Delete a `Recipient`
deleteRecipient
    :: RecipientId   -- ^ `RecipiendId` of `Recipient` to delete
    -> StripeRequest DeleteRecipient
deleteRecipient
   recipientid = request
  where request = mkStripeRequest DELETE url params
        url     = "recipients" </> getRecipientId recipientid
        params  = []

data DeleteRecipient
type instance StripeReturn DeleteRecipient = StripeDeleteResult

------------------------------------------------------------------------------
-- | Retrieve multiple 'Recipient's
getRecipients
    :: StripeRequest GetRecipients
getRecipients
  = request
  where request = mkStripeRequest GET url params
        url     = "recipients"
        params  = []

data GetRecipients
type instance StripeReturn GetRecipients = StripeList Recipient
instance StripeHasParam GetRecipients ExpandParams
instance StripeHasParam GetRecipients (EndingBefore RecipientId)
instance StripeHasParam GetRecipients Limit
instance StripeHasParam GetRecipients (StartingAfter RecipientId)
instance StripeHasParam GetRecipients IsVerified
