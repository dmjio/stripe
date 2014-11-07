{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.Stripe.Types.Util
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.Stripe.Types.Util
       ( -- * Getters
         getTransactionId
       , getCustomerId
       , getCardId
       , getRecipientCardId
       , getRecipientId
       , getAccountId
       , getChargeId
       , getInvoiceId
       , getInvoiceItemId
       ) where

import           Data.Text        (Text)
import           Web.Stripe.Types
------------------------------------------------------------------------------
-- | Helper for retrieving `TransactionId`
getTransactionId :: TransactionId -> Text
getTransactionId (TransactionId x) = x
getTransactionId (ExpandedTransaction BalanceTransaction { balanceTransactionId = TransactionId x }) = x
getTransactionId _ = ""

------------------------------------------------------------------------------
-- | Helper for retrieving `CustomerId`
getCustomerId :: CustomerId -> Text
getCustomerId (CustomerId x) = x
getCustomerId (ExpandedCustomer Customer { customerId = CustomerId x }) = x
getCustomerId _ = ""

------------------------------------------------------------------------------
-- | Helper for retrieving `CardId`
getCardId :: CardId -> Text
getCardId (CardId x) = x
getCardId (ExpandedCard Card { cardId = CardId x }) = x
getCardId _ = ""

------------------------------------------------------------------------------
-- | Helper for retrieving `RecipientCardId`
getRecipientCardId :: RecipientCardId -> Text
getRecipientCardId (RecipientCardId x) = x
getRecipientCardId (ExpandedRecipientCard RecipientCard { recipientCardId = RecipientCardId x }) = x
getRecipientCardId _ = ""

------------------------------------------------------------------------------
-- | Helper for retrieiving `RecipientId`
getRecipientId :: RecipientId -> Text
getRecipientId (RecipientId x) = x
getRecipientId (ExpandedRecipient Recipient { recipientId = RecipientId x }) = x
getRecipientId _ = ""

------------------------------------------------------------------------------
-- | Helper for retrieving `AccountId`
getAccountId :: AccountId -> Text
getAccountId (AccountId x) = x
getAccountId (ExpandedAccount Account { accountId = AccountId x }) = x
getAccountId _ = ""

------------------------------------------------------------------------------
-- | Helper for retrieving `CardId`
getChargeId :: ChargeId -> Text
getChargeId (ChargeId x) = x
getChargeId (ExpandedCharge Charge { chargeId = ChargeId x }) = x
getChargeId _ = ""

------------------------------------------------------------------------------
-- | Helper for retrieving `InvoiceId`
getInvoiceId :: InvoiceId -> Text
getInvoiceId (InvoiceId x) = x
getInvoiceId (ExpandedInvoice Invoice { invoiceId = Nothing }) = ""
getInvoiceId (ExpandedInvoice Invoice { invoiceId = Just (InvoiceId x) }) = x
getInvoiceId _ = ""

------------------------------------------------------------------------------
-- | Helper for retrieving `InvoiceItemId`
getInvoiceItemId :: InvoiceItemId -> Text
getInvoiceItemId (InvoiceItemId x) = x
getInvoiceItemId (ExpandedInvoiceItem InvoiceItem { invoiceItemId = InvoiceItemId x }) = x
getInvoiceItemId _ = ""
