# LiBro backend

[![Build and test](https://github.com/libro-app/backend/actions/workflows/test.yml/badge.svg)](https://github.com/libro-app/backend/actions/workflows/test.yml)
[![Publish API docs](https://github.com/libro-app/backend/actions/workflows/haddock-pages.yml/badge.svg)](https://github.com/libro-app/backend/actions/workflows/haddock-pages.yml)

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
