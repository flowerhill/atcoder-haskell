PROJECT_NAME = my-contest
EXEC = main
SOURCE = app/Main.hs

DEBUG ?= 0
ifeq ($(DEBUG), 1)
    DEBUG_FLAGS = -DDEBUG -fforce-recomp
    BUILD_TYPE = debug
else
    DEBUG_FLAGS =
    BUILD_TYPE = release
endif

BASE_GHC_OPTIONS = -O2 -Wall -threaded -rtsopts -with-rtsopts=-N -funbox-strict-fields -fexcess-precision

build:
	cabal build --enable-optimization=2 --ghc-options="$(BASE_GHC_OPTIONS)"

build-debug:
	cabal build --enable-optimization=2 --ghc-options="$(BASE_GHC_OPTIONS) $(DEBUG_FLAGS)"

build-quiet:
	@cabal build > /dev/null 2>&1

test: build-quiet
	@echo "Running tests in $(BUILD_TYPE) mode..."
	oj test -c "cabal run"

test-debug:
	@echo "Running tests in debug mode..."
	$(MAKE) build-debug DEBUG=1
	oj test -c "cabal run"

test-case:
ifndef CASE
	$(error CASE is not set. Usage: make test-case CASE=1)
endif
	@echo "Running debug test with sample-$(CASE)..."
	$(MAKE) build-debug DEBUG=1
	cabal run < test/sample-$(CASE).in

submit-oj: test
	oj submit $(SOURCE) --language haskell

submit: test
	acc submit -- $(SOURCE) --language haskell

deps:
	cabal build --dependencies-only

clean:
	cabal clean

show-debug-info:
	@echo "DEBUG: $(DEBUG)"
	@echo "DEBUG_FLAGS: $(DEBUG_FLAGS)"
	@echo "BUILD_TYPE: $(BUILD_TYPE)"
	@echo "GHC_OPTIONS: $(BASE_GHC_OPTIONS) $(DEBUG_FLAGS)"

help:
	@echo "Available targets:"
	@echo "  build              - Build release version"
	@echo "  build-debug        - Build debug version"
	@echo "  test-quiet         - Run tests (release, silent build)"
	@echo "  test               - Run tests (release)"
	@echo "  test-debug         - Run tests (debug)"
	@echo "  test-case CASE=N   - Run specific test case (debug)"
	@echo "  submit             - Submit with acc"
	@echo "  submit-oj          - Submit with oj"
	@echo "  clean              - Clean build artifacts"
	@echo "  deps               - Install dependencies"
	@echo "  show-debug-info    - Show debug configuration"
	@echo ""
	@echo "Debug mode: make <target> DEBUG=1"
	@echo "Example: make test-debug"

.PHONY: build build-debug build-quiet test test-debug test-case submit-oj submit deps clean show-debug-info help
