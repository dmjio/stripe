{-# LANGUAGE OverloadedStrings #-}
module Web.Stripe.ApplicationFee
    (  -- * API
      getApplicationFee
    , getApplicationFees
       -- * Types
    , ApplicationId  (..)
    , ApplicationFee (..)
    , FeeId          (..)
    , StripeList     (..)
    , EndingBefore
    , StartingAfter
    , Limit
    ) where

import           Web.Stripe.Client.Internal (Method (GET), Stripe,
                                             StripeRequest (..), callAPI,
                                             getParams, toText, (</>))
import           Web.Stripe.Types           (ApplicationFee (..),
                                             ApplicationId (..), 
                                             EndingBefore, FeeId (..),
                                             Limit, StartingAfter,
                                             StripeList (..))

------------------------------------------------------------------------------
-- | 'ApplicationFee' retrieval
getApplicationFee
    :: FeeId -- ^ The FeeID associated with the application
    -> Stripe ApplicationFee
getApplicationFee
    (FeeId feeid) = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees" </> feeid
        params  = []

------------------------------------------------------------------------------
-- | 'ApplicationFee's retrieval
getApplicationFees
    :: Maybe Limit         -- ^ Defaults to 10 if `Nothing` specified
    -> StartingAfter FeeId -- ^ Paginate starting after the following `FeeId`
    -> EndingBefore FeeId  -- ^ Paginate ending before the following `FeeId`
    -> Stripe (StripeList ApplicationFee)
getApplicationFees
    limit
    startingAfter
    endingBefore  = callAPI request
  where request = StripeRequest GET url params
        url     = "application_fees"
        params  = getParams [
            ("limit", toText `fmap` limit )
          , ("starting_after", (\(FeeId x) -> x) `fmap` startingAfter)
          , ("ending_before", (\(FeeId x) -> x) `fmap` endingBefore)
          ]


