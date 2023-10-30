# LiBro backend

[![build, test, docs](https://github.com/libro-app/backend/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/libro-app/backend/actions/workflows/haskell-ci.yml)

## Dependencies

System dependencies:

- Modern GHC
- LibreOffice
- libexpat1-dev (Ubuntu) / expat-devel (Fedora)
- libbz2-dev (Ubuntu) / bzip2-devel (Fedora)

Haskell dependencies:

```
cabal install --only-dependencies all
```

## Run tests
 
Running all the tests with `make test` may take some time. Run individual tests with
 
```
$ cabal test --test-show-details=direct --test-options='--match="RESTful JSON web service"'
$ make test_only pattern="SafeText wrapper/Safe packing"
```

## Author and license

Copyright (c) 2023 Mirko Westermeier

Released under the MIT license (see LICENSE) for details.
