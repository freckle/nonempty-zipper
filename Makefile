all: setup build test lint

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install --copy-compiler-tool hlint weeder

.PHONY: build
build:
	stack build --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build --fast --pedantic --test

.PHONY: lint
lint:
	stack exec hlint
	stack exec weeder

.PHONY: clean
clean:
	stack clean

.PHONY: check-nightly
check-nightly: STACK_ARGUMENTS=--stack-yaml stack-nightly.yaml --resolver nightly
check-nightly: setup build test

NONEMPTY_ZIPPER_VERSION = \
  v$(shell sed '/^version: \(.*\)$$/!d; s//\1/' ./package.yaml)

## Release and tag bcp47 at the version in package.yaml
.PHONY: release.nonempty-zipper
release.bcp47: RELEASE_PACKAGE=nonempty-zipper
release.bcp47: RELEASE_VERSION=$(NONEMPTY_ZIPPER_VERSION)
release.bcp47: release

RELEASE_PACKAGE ?=
RELEASE_VERSION ?=

## Release $(RELEASE_PACKAGE) at $(RELEASE_VERSION)
.PHONY: release
release:
	[ -n "$(RELEASE_PACKAGE)" ]
	[ -n "$(RELEASE_VERSION)" ]
	stack upload --pvp-bounds both "$(RELEASE_PACKAGE)"
	git tag --sign --message \
	  "$(RELEASE_PACKAGE)-$(RELEASE_VERSION)" \
	  "$(RELEASE_PACKAGE)-$(RELEASE_VERSION)"
	git push --follow-tags
