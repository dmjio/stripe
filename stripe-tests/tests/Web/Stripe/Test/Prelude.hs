{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.Prelude
       ( ($)
       , (-&-)
       , Char
       , Functor
       , IO
       , String
       , error
       , module GHC.Num
       , id
       , (.)
       , length
       , undefined
       , return
       , (>>=)
       , (>>)
       , fail
       , void
       , liftIO
       , fromString
       , stripeLift
       , module Test.Hspec
       , Eq(..)
       , Bool(..)
       , Maybe(..)
       , Stripe
       , StripeRequestF(..)
       , StripeSpec
       ) where

import           Data.Aeson      (Value, Result(..), FromJSON, fromJSON)
import           Data.Either     (Either)
import           Data.String     (fromString)
import           Data.Maybe      (Maybe(..))
import           GHC.Num         (fromInteger)
import           Prelude         (Bool(..), Eq(..), Functor(..), ($), IO, Char, String, error, undefined, (.), id, length)
import           Test.Hspec
import           Test.Hspec.Core.Spec (SpecM)
import qualified Control.Monad       as M
import qualified Control.Monad.Fail  as MF
import qualified Control.Monad.Trans as M
import           Control.Monad.Trans.Free (FreeT(..), liftF)
import           Web.Stripe.Client

------------------------------------------------------------------------------
-- Stripe free monad

data StripeRequestF ret = forall req. StripeRequestF
    { getStripeRequest :: StripeRequest req
    , decode           :: Value -> Result ret
    }

instance Functor StripeRequestF where
  fmap f (StripeRequestF req d) = StripeRequestF req (fmap f . d)

toStripeRequestF
    :: (FromJSON ret, StripeReturn req ~ ret)
    => StripeRequest req
    -> StripeRequestF ret
toStripeRequestF (StripeRequest m e q) =
  StripeRequestF (StripeRequest m e q) fromJSON

type Stripe = FreeT StripeRequestF IO
type StripeSpec = (forall a. Stripe a -> IO (Either StripeError a)) -> Spec

------------------------------------------------------------------------------
-- A class which lifts 'StripeRequest a' to the 'Stripe' monad and
-- leaves everything else alone.
--
class StripeLift a where
  type LiftedType a
  stripeLift :: a -> (LiftedType a)

instance (FromJSON (StripeReturn req)) => StripeLift (StripeRequest req) where
    type LiftedType (StripeRequest req) = Stripe (StripeReturn req)
    stripeLift req = liftF $ toStripeRequestF req

instance StripeLift (Stripe a) where
  type LiftedType (Stripe a) = Stripe a
  stripeLift = id

instance StripeLift (IO a) where
  type LiftedType (IO a) = IO a
  stripeLift = id

instance StripeLift (SpecM a r) where
  type LiftedType (SpecM a r) = SpecM a r
  stripeLift = id

------------------------------------------------------------------------------
-- hack the do-syntax and related functions to automatically turn
-- StripeReq values into monadic functions.
--
-- This is useful in the test suite where we a running a bunch of
-- back-to-back stripe transactions with little business logic in
-- between.

(>>=) :: (StripeLift t, M.Monad m, LiftedType t ~ m a) =>
         t -> (a -> m b) -> m b
m >>= f = (stripeLift m) M.>>= f

(>>) :: (StripeLift t, M.Monad m, LiftedType t ~ m a) => t -> m b -> m b
(>>) m n = m >>= \_ -> n

void :: (FromJSON (StripeReturn a)) => StripeRequest a -> Stripe ()
void req = M.void (stripeLift req)

fail :: (MF.MonadFail m) => String -> m a
fail = MF.fail

return :: (M.Monad m) => a -> m a
return = M.return

liftIO :: IO a -> Stripe a
liftIO io = M.liftIO io
