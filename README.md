stripe
========
![Hackage](https://img.shields.io/hackage/v/stripe-haskell.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/stripe-haskell.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![MIT License](http://img.shields.io/badge/license-MIT-brightgreen.svg)
![Build Status](https://api.travis-ci.org/dmjio/stripe.svg?branch=master)
[![Join the chat at https://gitter.im/dmjio/stripe](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/dmjio/stripe?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)


========
##Stripe API coverage for Haskell ([Stripe API](http://stripe.com/docs/api))  
  
All Stripe commands are supported, including but not limited to Charges, Refunds, Customers, Cards, Subscriptions, Plans, Coupons, Discounts, Invoices, Invoice Items, Disputes, Transfers, Recipients, Bitcoin, Application Fees, Application Fee Refunds, Account, Balance, Events and Tokens.
***
## Haddock Coverage
  All code written for this library is documented to completion with the haddock documentation tool

##[100+ Hspec Tests](https://github.com/dmjio/stripe-haskell/blob/master/COVERAGE.md)
 Thoroughly unit-tested with hspec.
    All API commands are unit-tested before inclusion into the API (see the tests directory).
    To run the tests, perform the following:
```bash    
cabal clean 
cabal configure --enable-tests
cabal build tests
dist/build/tests/tests # You will be prompted to enter your *TEST* key
```

##[Pagination](https://stripe.com/docs/api#pagination)
  Pagination is possible on all API calls that return a JSON array.    
  Any API call that returns a `StripeList` is eligible for pagination.
  To use in practice do the following:

```haskell  
import Web.Stripe
import Web.Stripe.Customer
    
main :: IO ()
main = do
  let config = StripeConfig "secret key"
  result <- stripe config $ getCustomers 
                                (Just 30 :: Maybe Limit) -- Defaults to 10 if Nothing, 100 is Max
                                (StartingAfter $ CustomerId "customer_id0")
                                (EndingBefore $ CustomerId "customer_id30")
  case result of
    Right stripelist -> print (list stripelist :: [Customer])
    Left stripeError -> print stripeError
```    

##[Versioning](https://stripe.com/docs/api#versioning)
  All versioning is hard-coded (for safety).
  Stripe API versions specified in the HTTP headers of Stripe requests take precedence 
  over the API version specified in your Stripe Dashboard. In an attempt to ensure
  API consistency and correct parsing of returned JSON, all Stripe versions are hard-coded, and are
  inaccessible to the end-users of this library. When a new Stripe API version is released 
  this library will increment the hard-coded API version.

##[Expansion](https://stripe.com/docs/api#expansion)
  Object expansion is supported on Stripe objects eligible for expansion though the `ExpandParams` type.
  Object expansion allows normal Stripe API calls to return expanded objects inside of other objects. 
  For example, a `Customer` object contains a Card ID hash on the default_card field.
  This default_card hash can be expanded into a full `Card` object inside a `Customer` object.
  As an example:

```haskell  
import Web.Stripe
import Web.Stripe.Customer
    
main :: IO ()
main = do
  let config = StripeConfig "secret key"
  result <- stripe config $ getCustomerExpandable 
                                   (CustomerId "customerid")
                                   (["default_card"] :: ExpandParams)
  case result of
    Right customer -> print (defaultCard customer) -- Will be an `ExpandedCard`
    Left stripeError -> print stripeError
```

##[MetaData](https://stripe.com/docs/api#metadata)
  Stripe objects allow the embedding of arbitrary metadata.
  Any Stripe object that supports the embedding of metadata is available via this API.
  As an example:
  
```haskell
import Web.Stripe
import Web.Stripe.Coupon
  
main :: IO ()
main = do
  let config = StripeConfig "secret key"
  result <- stripe config $ updateCoupon (CouponId "couponid") [("key1", "value2"), ("key2", "value2")]
  case result of
    Right coupon -> print $ couponMetaData coupon
    Left stripeError -> print stripeError
```

##[Issues](https://github.com/dmjio/stripe-haskell/issues)
  Any API recommendations or bugs can be reported on the GitHub issue tracker.
  Pull requests welcome!
