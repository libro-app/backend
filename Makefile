.PHONY: all build warnings test test_only doc serve stats

all: build test doc stats

build:
	cabal build --enable-tests all

warnings:
	cabal build --enable-tests --ghc-options="-fforce-recomp" all

test:
	cabal test --test-show-details=direct

test_only:
	cabal test --test-show-details=direct --test-options='--match="$(pattern)"'

doc:
	cabal haddock --haddock-hyperlinked-source --haddock-html-location='https://hackage.haskell.org/package/$$pkg-$$version/docs'

serve:
	cabal run libro-backend

stats:
	find lib -name '*.hs' -not -path "./dist-newstyle/*" | sort | xargs wc -l
	find test -name '*.hs' -not -path "./dist-newstyle/*" | sort | xargs wc -l
	find -name '*.hs' -not -path "./dist-newstyle/*" | xargs wc -l | tail -1
