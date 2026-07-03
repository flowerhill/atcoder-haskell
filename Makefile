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

BUNDLE = dist/Submit.hs

build:
	cabal build --enable-optimization=2 --ghc-options="$(BASE_GHC_OPTIONS)"

build-debug:
	cabal build --enable-optimization=2 --ghc-options="$(BASE_GHC_OPTIONS) $(DEBUG_FLAGS)"

build-quiet:
	@cabal build > /dev/null 2>&1

test: build-quiet
	@echo "Running tests in $(BUILD_TYPE) mode..."
	oj test -c "cabal run main"

test-bundle: test bundle

unit-test:
	cabal test

test-debug:
	@echo "Running tests in debug mode..."
	$(MAKE) build-debug DEBUG=1
	oj test -c "cabal run main"

test-case:
ifndef CASE
	$(error CASE is not set. Usage: make test-case CASE=1)
endif
	@echo "Running debug test with sample-$(CASE)..."
	$(MAKE) build-debug DEBUG=1
	cabal run main < test/sample-$(CASE).in

test-all: test doctest
	@echo "✓ All tests passed!"

# サンプルテスト + 提出ファイル作成まで（実際の提出はしない）
# oj submit が壊れているとき、これで dist/Submit.hs を用意して手動提出する
submit-file: test-bundle
	@echo "→ $(BUNDLE) を手動で提出してください"

submit-oj: test-bundle
	@oj submit $(BUNDLE) --language haskell

submit: test-bundle
	@acc submit $(BUNDLE) --language haskell

deps:
	cabal build --dependencies-only

clean:
	cabal clean
	rm -rf $(BUNDLE)

doctest:
	./scripts/doctest.sh

# 単一ファイル生成のみ（format/verify の前段）
gen-bundle:
	cabal run bundle --verbose=0 -- src app/Main.hs > $(BUNDLE)

format: gen-bundle
	@ormolu --mode inplace $(BUNDLE)

verify-bundle: gen-bundle
	@cabal exec -- ghc -fno-code $(BUNDLE) || (echo "Bundle has syntax errors!" && exit 1)

# 生成 → format → 型検査 までまとめて実行し、提出ファイルを完成させる
bundle: gen-bundle format verify-bundle
	@echo "✓ 提出ファイル $(BUNDLE) を作成しました"

show-debug-info:
	@echo "DEBUG: $(DEBUG)"
	@echo "DEBUG_FLAGS: $(DEBUG_FLAGS)"
	@echo "BUILD_TYPE: $(BUILD_TYPE)"
	@echo "GHC_OPTIONS: $(BASE_GHC_OPTIONS) $(DEBUG_FLAGS)"

help:
	@echo "Available targets:"
	@echo ""
	@echo " Build:"
	@echo "  build              - Build release version"
	@echo "  build-debug        - Build debug version"
	@echo "  deps               - Install dependencies only"
	@echo ""
	@echo " Test:"
	@echo "  test               - Run sample tests (release)"
	@echo "  test-debug         - Run sample tests (debug)"
	@echo "  test-case CASE=N   - Run specific sample case (debug)"
	@echo "  unit-test          - Run cabal test suite"
	@echo "  doctest            - Run doctests"
	@echo "  test-all           - test + doctest"
	@echo ""
	@echo " Bundle (submit file = $(BUNDLE)):"
	@echo "  gen-bundle         - Generate single-file bundle only"
	@echo "  format             - Format the bundle in place (needs ormolu)"
	@echo "  verify-bundle      - Type-check the bundle (ghc -fno-code)"
	@echo "  bundle             - gen-bundle + format + verify-bundle"
	@echo "  test-bundle        - test + bundle"
	@echo ""
	@echo " Submit:"
	@echo "  submit-file        - test-bundle, then stop (submit manually)"
	@echo "  submit             - test-bundle + acc submit"
	@echo "  submit-oj          - test-bundle + oj submit"
	@echo ""
	@echo " Misc:"
	@echo "  clean              - Clean build artifacts + bundle"
	@echo "  show-debug-info    - Show debug configuration"
	@echo ""
	@echo "Debug mode: make <target> DEBUG=1"
	@echo "Example: make test-debug"

.PHONY: build build-debug build-quiet test test-bundle unit-test test-debug test-case test-all submit-file submit-oj submit deps clean doctest gen-bundle format bundle verify-bundle show-debug-info help
