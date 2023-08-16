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

## Author and license

Copyright (c) 2023 Mirko Westermeier

Released under the MIT license (see LICENSE) for details.
