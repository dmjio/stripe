{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.Card
    ( -- * API
      -- ** Customers
      -- *** Create Customer Card
      createCustomerCard
    , createCustomerCardByToken
      -- *** Update Customer Card
    , updateCustomerCard
      -- *** Get Customer Card(s)
    , getCustomerCard
    , getCustomerCards
      -- *** Delete Card
    , deleteCustomerCard
      -- ** Recipients
      -- *** Create Recipient Card
    , createRecipientCard
    , createRecipientCardByToken
      -- *** Get Recipient Card(s)
    , getRecipientCard
    , getRecipientCards
      -- *** Updated Recipient Card
    , updateRecipientCard
      -- *** Delete Recipient Card
    , deleteRecipientCard
      -- * Types
    , Brand          (..)
    , Card           (..)
    , CardId         (..)
    , CardNumber     (..)
    , ExpMonth       (..)
    , ExpYear        (..)
    , CVC            (..)
    , Name
    , AddressLine1   (..)
    , AddressLine2   (..)
    , AddressCity    (..)
    , AddressCountry (..)
    , AddressState   (..)
    , AddressZip     (..)
    , RecipientId    (..)
    , RecipientCard  (..)
    ) where

import           Control.Applicative        ((<$>))
import           Data.Aeson                 (FromJSON)
import           Web.Stripe.Client.Internal
import           Web.Stripe.Types

------------------------------------------------------------------------------
-- | Base request function for `Card` creation, good for making custom create `Card` functions
createCardBase
  :: FromJSON a
  => URL                  -- ^ The `Recipient` or `Customer` on which to make the `Card` value can be "recipients" or "customers" only
  -> ID                   -- ^ The `RecipientId` or `CustomerId` for which the `Card` can be made
  -> Maybe TokenId        -- ^ The `TokenId` to be used for `Card` creation
  -> Maybe CardNumber     -- ^ `CardNumber` for `Card` creation
  -> Maybe ExpMonth       -- ^ Expiration Month for `Card` creation
  -> Maybe ExpYear        -- ^ Expiration Year for `Card` creation
  -> Maybe CVC            -- ^ CVC for `Card` creation
  -> Maybe Name           -- ^ Name of `Recipient` or `Customer` to be used for `Card`
  -> Maybe AddressCity    -- ^ City associated with `Card`
  -> Maybe AddressCountry -- ^ Country associated with `Card`
  -> Maybe AddressLine1   -- ^ Address Line 1 associated with `Card`
  -> Maybe AddressLine2   -- ^ Address Line 2 associated with `Card`
  -> Maybe AddressState   -- ^ Address State 2 associated with `Card`
  -> Maybe AddressZip     -- ^ Address Zip associated with `Card`
  -> MetaData             -- ^ MetaData for `Card`
  -> Stripe a
createCardBase
    requestType
    requestId
    tokenId
    cardNumber
    expMonth
    expYear
    cvc
    name
    addressCity
    addressCountry
    addressLine1
    addressLine2
    addressState
    addressZip
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = requestType </> requestId </> "cards"
        params  = toMetaData metadata ++ getParams [
                     ("card", (\(TokenId x) -> x) <$> tokenId)
                   , ("card[number]", (\(CardNumber x) -> x) <$> cardNumber)
                   , ("card[exp_month]", (\(ExpMonth x) -> toText x) <$> expMonth)
                   , ("card[exp_year]", (\(ExpYear x) -> toText x) <$> expYear)
                   , ("card[cvc]", (\(CVC x) -> x) <$> cvc)
                   , ("name", name)
                   , ("address_city", (\(AddressCity x) -> x) <$> addressCity)
                   , ("address_country", (\(AddressCountry x) -> x) <$> addressCountry)
                   , ("address_line1", (\(AddressLine1 x) -> x) <$> addressLine1 )
                   , ("address_line2", (\(AddressLine2 x) -> x) <$> addressLine2 )
                   , ("address_state", (\(AddressState x) -> x) <$> addressState )
                   , ("address_zip", (\(AddressZip x) -> x) <$> addressZip )
                  ]

------------------------------------------------------------------------------
-- | Create a `Customer` card using a `Token`
createCustomerCardByToken
    :: CustomerId -- ^ The Customer to which the card will be added
    -> TokenId    -- ^ The Token representative of the card
    -> Stripe Card
createCustomerCardByToken
    (CustomerId customerId)
    tokenId = createCardBase "customers" customerId (Just tokenId)
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing []

------------------------------------------------------------------------------
-- | Create a `Recipient` card using a `Token`
createRecipientCardByToken
    :: RecipientId -- ^ The Customer to which the card will be added
    -> TokenId     -- ^ The Token representative of the card
    -> Stripe RecipientCard
createRecipientCardByToken
    (RecipientId recipientId)
    tokenId = createCardBase "customers" recipientId (Just tokenId)
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing []

------------------------------------------------------------------------------
-- | Create a `Customer` card by `CardNumber`
createCustomerCard
    :: CustomerId -- ^ `Customer` to which the card will be added
    -> CardNumber -- ^ `Card` digits
    -> ExpMonth   -- ^ `Card` expiration month
    -> ExpYear    -- ^ `Card` expiration year
    -> CVC        -- ^ `Card` cvc number
    -> Stripe Card
createCustomerCard
    (CustomerId customerId)
    cardNumber
    expMonth
    expYear
    cvc = createCardBase "customers" customerId Nothing
              (Just cardNumber) (Just expMonth) (Just expYear)
              (Just cvc) Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing []

------------------------------------------------------------------------------
-- | Create a `Recipient` `Card` by `CardNumber`
createRecipientCard
    :: RecipientId -- ^ `Recipient` to which the card will be added
    -> CardNumber  -- ^ `Card` digits
    -> ExpMonth    -- ^ `Card` expiration month
    -> ExpYear     -- ^ `Card` expiration year
    -> CVC         -- ^ `Card` cvc number
    -> Stripe RecipientCard
createRecipientCard
    (RecipientId recipientId)
    cardNumber
    expMonth
    expYear
    cvc = createCardBase "recipients" recipientId Nothing
              (Just cardNumber) (Just expMonth) (Just expYear)
              (Just cvc) Nothing Nothing
              Nothing Nothing Nothing
              Nothing Nothing []

------------------------------------------------------------------------------
-- | Update a `Card`, any fields not specified will remain the same
updateCardBase
    :: FromJSON a
    => URL                  -- ^ The `Recipient` or `Customer` on which to make the `Card` value can be "recipients" or "customers" only
    -> ID                   -- ^ The `RecipientId` or `CustomerId` for which the `Card` can be made
    -> CardId               -- ^ The `CardId` associated with the `Card` to be updated
    -> Maybe Name           -- ^ Name of `Recipient` or `Customer` to be used for `Card`
    -> Maybe AddressCity    -- ^ City associated with `Card`
    -> Maybe AddressCountry -- ^ Country associated with `Card`
    -> Maybe AddressLine1   -- ^ Address Line 1 associated with `Card`
    -> Maybe AddressLine2   -- ^ Address Line 2 associated with `Card`
    -> Maybe AddressState   -- ^ Address State 2 associated with `Card`
    -> Maybe AddressZip     -- ^ Address Zip associated with `Card`
    -> MetaData             -- ^ MetaData for `Card`
    -> Stripe a
updateCardBase
    requestType
    requestId
    (CardId cardId)
    name
    addressCity
    addressCountry
    addressLine1
    addressLine2
    addressState
    addressZip 
    metadata    = callAPI request
  where request = StripeRequest POST url params
        url     = requestType </> requestId </> "cards" </> cardId
        params  = toMetaData metadata ++ getParams [
                     ("name", name)
                   , ("address_city", (\(AddressCity x) -> x) <$> addressCity)
                   , ("address_country", (\(AddressCountry x) -> x) <$> addressCountry)
                   , ("address_line1", (\(AddressLine1 x) -> x) <$> addressLine1 )
                   , ("address_line2", (\(AddressLine2 x) -> x) <$> addressLine2 )
                   , ("address_state", (\(AddressState x) -> x) <$> addressState )
                   , ("address_zip", (\(AddressZip x) -> x) <$> addressZip )
                   ]

------------------------------------------------------------------------------
-- | Update a `Customer` `Card`
updateCustomerCard
    :: CustomerId           -- ^ `CustomerId` associated with the `Card` to be updated
    -> CardId               -- ^ The `CardId` associated with the `Card` to be updated
    -> Maybe Name           -- ^ Name of `Recipient` or `Customer` to be used for `Card`
    -> Maybe AddressCity    -- ^ City associated with `Card`
    -> Maybe AddressCountry -- ^ Country associated with `Card`
    -> Maybe AddressLine1   -- ^ Address Line 1 associated with `Card`
    -> Maybe AddressLine2   -- ^ Address Line 2 associated with `Card`
    -> Maybe AddressState   -- ^ Address State 2 associated with `Card`
    -> Maybe AddressZip     -- ^ Address Zip associated with `Card`
    -> MetaData             -- ^ MetaData for `Card`
    -> Stripe Card
updateCustomerCard
    (CustomerId customerId)  = updateCardBase "customers" customerId 

------------------------------------------------------------------------------
-- | Update a `Recipient` `Card`
updateRecipientCard
    :: RecipientId          -- ^ `RecipientId` associated with the `Card` to be updated
    -> CardId               -- ^ The `CardId` associated with the `Card` to be updated
    -> Maybe Name           -- ^ Name of `Recipient` or `Customer` to be used for `Card`
    -> Maybe AddressCity    -- ^ City associated with `Card`
    -> Maybe AddressCountry -- ^ Country associated with `Card`
    -> Maybe AddressLine1   -- ^ Address Line 1 associated with `Card`
    -> Maybe AddressLine2   -- ^ Address Line 2 associated with `Card`
    -> Maybe AddressState   -- ^ Address State 2 associated with `Card`
    -> Maybe AddressZip     -- ^ Address Zip associated with `Card`
    -> MetaData             -- ^ MetaData for `Card`
    -> Stripe Card
updateRecipientCard
    (RecipientId recipientId)  = updateCardBase "recipients" recipientId 

------------------------------------------------------------------------------
-- | Base Request for retrieving cards from either a `Customer` or `Recipient`
getCardBase
    :: URL    -- ^ The type of the request to support (recipient or customer)
    -> ID     -- ^ `CustomerId` or `RecipientId` of the `Card` to retrieve
    -> CardId -- ^ `CardId` of the card to retrieve
    -> Stripe Card
getCardBase
    requestType
    requestId
    (CardId cardId) = callAPI request
  where request = StripeRequest GET url params
        url     = requestType </> requestId </> "cards" </> cardId
        params  = []

------------------------------------------------------------------------------
-- | Get card by `CustomerId` and `CardId`
getCustomerCard
    :: CustomerId -- ^ `CustomerId` of the `Card` to retrieve
    -> CardId     -- ^ `CardId` of the card to retrieve
    -> Stripe Card
getCustomerCard
    (CustomerId customerId) = getCardBase "customers" customerId

------------------------------------------------------------------------------
-- | Get card by `RecipientId` and `CardId`
getRecipientCard
    :: RecipientId -- ^ `RecipientId` of the `Card` to retrieve
    -> CardId      -- ^ `CardId` of the card to retrieve
    -> Stripe Card
getRecipientCard
       (RecipientId recipientId) = getCardBase "recipients" recipientId

------------------------------------------------------------------------------
-- | Base Request for retrieving `Customer` or `Recipient` cards
getCardsBase
    :: FromJSON a
    => URL  -- ^ The type of the request to support (`Recipient` or `Customer`)
    -> ID   -- ^ `CustomerId` or `RecipientId` of the `Card` to retrieve
    -> Maybe Limit          -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter CardId -- ^ Paginate starting after the following `CardId`
    -> EndingBefore CardId  -- ^ Paginate ending before the following `CardId`
    -> Stripe (StripeList a)
getCardsBase
    requestType
    requestId
    limit
    startingAfter
    endingBefore = callAPI request
  where request = StripeRequest GET url params
        url     = requestType </> requestId </> "cards"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(CardId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(CardId x) -> x) `fmap` endingBefore)
          ]

------------------------------------------------------------------------------
-- | Retrieve all cards associated with a `Customer`
getCustomerCards
    :: CustomerId   -- ^ The `CustomerId` associated with the cards
    -> Maybe Limit          -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter CardId -- ^ Paginate starting after the following `CardId`
    -> EndingBefore CardId  -- ^ Paginate ending before the following `CardId`
    -> Stripe (StripeList Card)
getCustomerCards
    (CustomerId customerId) = getCardsBase "customers" customerId

------------------------------------------------------------------------------
-- | Retrieve all cards associated with a `Recipient`
getRecipientCards
    :: RecipientId   -- ^ The `RecipientId` associated with the cards
    -> Maybe Limit          -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter CardId -- ^ Paginate starting after the following `CardId`
    -> EndingBefore CardId  -- ^ Paginate ending before the following `CardId`
    -> Stripe (StripeList RecipientCard)
getRecipientCards
    (RecipientId recipientId) = getCardsBase "recipients" recipientId

------------------------------------------------------------------------------
-- | Base request for `Card` removal from a `Customer` or `Recipient`
deleteCardBase
    :: URL    -- ^ The type of the request to support (`Recipient` or `Customer`)
    -> ID     -- ^ `CustomerId` or `RecipientId` of the `Card` to retrieve
    -> CardId -- ^ `CardId` associated with `Card` to be deleted
    -> Stripe StripeDeleteResult
deleteCardBase
    requestType
    requestId
    (CardId cardId) = callAPI request
  where request = StripeRequest DELETE url params
        url     = requestType </> requestId </> "cards" </> cardId
        params  = []

------------------------------------------------------------------------------
-- | Removes a card from a `Customer`
deleteCustomerCard
    :: CustomerId -- ^ The `CustomerId` associated with the `Card` to be removed
    -> CardId     -- ^ The `CardId` of the `Card` to be removed
    -> Stripe StripeDeleteResult
deleteCustomerCard
    (CustomerId customerId) = deleteCardBase "customers" customerId

------------------------------------------------------------------------------
-- | Removes a card from a `Customer`
deleteRecipientCard
    :: RecipientId -- ^ The `RecipientId` associated with the `Card` to be removed
    -> CardId      -- ^ The `CardId` of the `Card` to be removed
    -> Stripe StripeDeleteResult
deleteRecipientCard
    (RecipientId recipientId) = deleteCardBase "recipients" recipientId
