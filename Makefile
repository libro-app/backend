.PHONY: all build test doc stats

all: build test doc stats

build:
	cabal build --enable-tests --enable-benchmarks all

test:
	cabal test --test-show-details=direct

doc:
	cabal haddock --haddock-hyperlinked-source --haddock-html-location='https://hackage.haskell.org/package/$$pkg-$$version/docs'

stats:
	find lib -name '*.hs' -not -path "./dist-newstyle/*" | xargs wc -l
	find test -name '*.hs' -not -path "./dist-newstyle/*" | xargs wc -l
	find -name '*.hs' -not -path "./dist-newstyle/*" | xargs wc -l | tail -1
