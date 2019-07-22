To setup your development environment, you'll need the [Nix package manager](https://nixos.org/nix/).

1. Clone the repo:
   ```
   git clone git@github.com:dmjio/stripe.git
   cd stripe
   ```
2. Get into a shell for stripe-http-client (stripe-tests is used in the test suite of stripe-http-client).
   ```
   nix-shell --attr stripe-http-client.env
   cd stripe-http-client
   ```
3. Ensure dependencies are all correct (this is contingent on your nixpkgs
   version). Bump the upper bounds in the cabal files if need be.
4. Configure and run tests using cabal.
   ```
   cabal configure --enable-tests
   cabal test
   ```
