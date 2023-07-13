.PHONY: all build test doc

all: build test doc

build:
	cabal build --enable-tests --enable-benchmarks all

test:
	cabal test --test-show-details=direct

doc:
	cabal haddock --haddock-hyperlinked-source --haddock-html-location='https://hackage.haskell.org/package/$$pkg-$$version/docs'
