{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
module Web.Stripe.Test.Prelude
       ( ($)
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
       , StripeSpec
       ) where

import           Data.Either     (Either)
import           Data.String     (fromString)
import           Data.Maybe      (Maybe(..))
import           GHC.Num         (fromInteger)
import           Prelude         (Bool(..), Eq(..), Functor(..), ($), IO, Char, String, error, undefined, (.), id, length)
import           Test.Hspec
import           Test.Hspec.Core (SpecM)
import qualified Control.Monad   as M
import qualified Control.Monad.Trans as M
import           Control.Monad.Trans.Free (FreeT(..), liftF)
import           Web.Stripe

------------------------------------------------------------------------------
-- Stripe free monad
type Stripe  = FreeT StripeRequest IO

type StripeSpec = (forall a. Stripe a -> IO (Either StripeError a)) -> Spec

------------------------------------------------------------------------------
-- A class which lifts 'StripeRequest a' to the 'Stripe' monad and
-- leaves everything else alone.
--
class StripeLift a where
  type LiftedType a
  stripeLift :: a -> (LiftedType a)

instance StripeLift (StripeRequest a) where
  type LiftedType (StripeRequest a) = Stripe a
  stripeLift req = liftF req

instance StripeLift (Stripe a) where
  type LiftedType (Stripe a) = Stripe a
  stripeLift = id

instance StripeLift (IO a) where
  type LiftedType (IO a) = IO a
  stripeLift = id

instance StripeLift (SpecM a) where
  type LiftedType (SpecM a) = SpecM a
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

void :: StripeRequest a -> Stripe ()
void req = M.void (liftF req)

fail :: (M.Monad m) => String -> m a
fail = M.fail

return :: (M.Monad m) => a -> m a
return = M.return

liftIO :: IO a -> Stripe a
liftIO io = M.liftIO io
