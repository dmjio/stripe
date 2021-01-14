{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------

module Web.Stripe.PaymentMethod
    ( -- * API
      -- ** Customers
      -- *** Create PaymentMethod
      CreatePaymentMethodByToken
    , createPaymentMethodByToken
    , CreatePaymentMethod
    , createPaymentMethod
    , AttachPaymentMethod
    , attachPaymentMethod
      -- *** Get PaymentMethod(s)
    , GetPaymentMethod
    , getPaymentMethod
    , GetCustomerPaymentMethods
    , getCustomerPaymentMethods {-
    , GetPaymentMethods
    , getPaymentMethods
      -- *** Update PaymentMethod
    , UpdatePaymentMethod
    , updatePaymentMethod
      -- *** Delete PaymentMethod -}
    , DetachPaymentMethod
    , detachPaymentMethod
      -- * Types
    , AddressLine1    (..)
    , AddressLine2    (..)
    , AddressCity     (..)
    , AddressCountry  (..)
    , AddressState    (..)
    , AddressZip      (..)
    , Brand           (..)
    , PaymentMethod            (..)
    , PaymentMethodId          (..)
    , PaymentMethodType        (..)
    , CardHash        (..)
    , CardNumber      (..)
    , CVC             (..)
    , EndingBefore    (..)
    , ExpandParams    (..)
    , ExpMonth        (..)
    , ExpYear         (..)
    , Limit           (..)
    , Name            (..)
    , StartingAfter   (..)
    ) where

import           Data.Text                (Text)
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (AddressLine1(..), AddressLine2(..)
                                          , AddressCity(..), AddressCountry(..)
                                          , AddressState(..), AddressZip(..)
                                          , Brand(..), PaymentMethod(..), PaymentMethodId(..)
                                          , PaymentMethodType(..), CardHash(..), CardToken(..)
                                          , CardNumber(..), CustomerId(..)
                                          , CVC(..), EndingBefore(..)
                                          , ExpandParams(..)
                                          , ExpMonth(..), ExpYear(..), ID
                                          , Limit(..), Name(..), NewCard(..)
                                          , StartingAfter(..)
                                          , StripeDeleteResult(..)
                                          , StripeList(..), TokenId(..), URL)
import           Web.Stripe.Types.Util    (getCustomerId)

createPaymentMethodByToken
    :: TokenId -- ^ `TokenId` of card to add
    -> PaymentMethodType
    -> StripeRequest CreatePaymentMethodByToken
createPaymentMethodByToken
  tokenid
  typ           = request
  where request = mkStripeRequest POST url params
        url     = "payment_methods"
        params  = toStripeParam (CardToken tokenid) $
                  toStripeParam typ
                  []

data CreatePaymentMethodByToken
type instance StripeReturn CreatePaymentMethodByToken = PaymentMethod

createPaymentMethod
    :: NewCard
    -> StripeRequest CreatePaymentMethod
createPaymentMethod
  newPaymentMethod       = request
  where request = mkStripeRequest POST url params
        url     = "payment_methods"
        params  = toStripeParam newPaymentMethod $
                  toStripeParam PaymentMethodTypeCard
                  []

data CreatePaymentMethod
type instance StripeReturn CreatePaymentMethod = PaymentMethod

attachPaymentMethod
    :: PaymentMethodId -- ^ `TokenId` of card to add
    -> CustomerId      -- ^ id of customer
    -> StripeRequest CreatePaymentMethodByToken
attachPaymentMethod
  paymentMethodId
  customer      = request
  where request = mkStripeRequest POST url params
        url     = "payment_methods" </> getPaymentMethodId paymentMethodId </> "attach"
        params  = toStripeParam customer $
                  []

data AttachPaymentMethod
type instance StripeReturn AttachPaymentMethod = PaymentMethod

getPaymentMethod
  :: PaymentMethodId
  -> StripeRequest GetPaymentMethod
getPaymentMethod
  pmid          = request
  where request = mkStripeRequest GET url params
        url     = "payment_methods" </> getPaymentMethodId pmid
        params  = []
data GetPaymentMethod
type instance StripeReturn GetPaymentMethod = PaymentMethod
-- instance StripeHasParam GetPaymentMethod ExpandParams

getCustomerPaymentMethods
    :: CustomerId -- ^ `CustomerId` of the `PaymentMethod` to retrieve
    -> StripeRequest GetCustomerPaymentMethods
getCustomerPaymentMethods
  customer      = request
  where request = mkStripeRequest GET url params
        url     = "payment_methods"
        params  = toStripeParam customer $
                  toStripeParam PaymentMethodTypeCard
                  []

data GetCustomerPaymentMethods
type instance StripeReturn GetCustomerPaymentMethods = StripeList PaymentMethod
--instance StripeHasParam GetCustomerPaymentMethods ExpandParams
{-
------------------------------------------------------------------------------
-- | INTERNAL: Generalized update a `PaymentMethod`
updatePaymentMethod
  :: URL
  -> ID
  -> Text -- ^ cardid
  -> StripeRequest a
updatePaymentMethod
  prefix
  id_
  cardid_       = request
  where request = mkStripeRequest POST url params
        url     = prefix </> id_ </>
                  "cards" </> cardid_
        params  = []

------------------------------------------------------------------------------
-- | Update a `Customer` `PaymentMethod`
updateCustomerPaymentMethod
    :: CustomerId -- ^ `CustomerId` of the card holder
    -> PaymentMethodId     -- ^ `PaymentMethodId` of card to update
    -> StripeRequest UpdateCustomerPaymentMethod
updateCustomerPaymentMethod
  customerid
  (PaymentMethodId cardid)
    = updatePaymentMethod "customers" (getCustomerId customerid) cardid

data UpdateCustomerPaymentMethod
type instance StripeReturn UpdateCustomerPaymentMethod = PaymentMethod
instance StripeHasParam UpdateCustomerPaymentMethod AddressLine1
instance StripeHasParam UpdateCustomerPaymentMethod AddressLine2
instance StripeHasParam UpdateCustomerPaymentMethod AddressCity
instance StripeHasParam UpdateCustomerPaymentMethod AddressZip
instance StripeHasParam UpdateCustomerPaymentMethod AddressState
instance StripeHasParam UpdateCustomerPaymentMethod AddressCountry
instance StripeHasParam UpdateCustomerPaymentMethod ExpMonth
instance StripeHasParam UpdateCustomerPaymentMethod ExpYear
instance StripeHasParam UpdateCustomerPaymentMethod Name
-}

detachPaymentMethod
    :: PaymentMethodId -- ^ `PaymentMethodId` associated with `PaymentMethod` to be deleted
    -> StripeRequest DetachPaymentMethod
detachPaymentMethod
    pmid        = request
  where request = mkStripeRequest POST url params
        url     = "payment_methods" </> getPaymentMethodId pmid </> "detach"
        params  = []

data DetachPaymentMethod
type instance StripeReturn DetachPaymentMethod = PaymentMethod
{-
------------------------------------------------------------------------------
-- | INTERNAL: Generalized retrieve all cards for `ID`
getPaymentMethods
    :: URL
    -> ID
    -> StripeRequest a
getPaymentMethods
    prefix
    id_
    = request
  where request = mkStripeRequest GET url params
        url     = prefix </> id_ </> "cards"
        params  = []

------------------------------------------------------------------------------
-- | Retrieve all cards associated with a `Customer`
getCustomerPaymentMethods
    :: CustomerId    -- ^ The `CustomerId` associated with the cards
    -> StripeRequest GetCustomerPaymentMethods
getCustomerPaymentMethods
    customerid
    = getPaymentMethods "customers" (getCustomerId customerid)

data GetCustomerPaymentMethods
type instance StripeReturn GetCustomerPaymentMethods = (StripeList PaymentMethod)
instance StripeHasParam GetCustomerPaymentMethods ExpandParams
instance StripeHasParam GetCustomerPaymentMethods (EndingBefore PaymentMethodId)
instance StripeHasParam GetCustomerPaymentMethods Limit
instance StripeHasParam GetCustomerPaymentMethods (StartingAfter PaymentMethodId)
-}