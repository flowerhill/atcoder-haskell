PROJECT_NAME = my-contest
EXEC = main
SOURCE = app/Main.hs

build:
	cabal build --enable-optimization=2

test: build
	oj test -c "cabal run"

submit-oj: test
	oj submit $(SOURCE) --language haskell

submit: test
	acc submit -- $(SOURCE) --language haskell

submit: submit-acc

clean:
	cabal clean

deps:
	cabal build --dependencies-only

.PHONY: build test submit-oj submit-acc submit clean deps