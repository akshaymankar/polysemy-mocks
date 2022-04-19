# Changelog for polysemy-mocks

## 0.3.0.0

* Support template-haskell-2.17.0 (and so GHC 9.0)
* Add 'HasCallStack' constraint to 'evalMocks', {run,eval,exec}Mock and default
  implementations for the "returns" functions (which throws error, by default).
  'RankNTypes' is now needed to use the TH mocks.

## 0.2.0.0

* Support polysemy >= 1.7, drop support for older versions

## 0.1.0.1

* Better docs

## 0.1.0.0

* First release. Can mock effects.
