##############################################################################
# Help
##############################################################################
.PHONY: help
help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html             to make standalone HTML files"
	@echo "  live_html        to interactively build/change the documentation"
	@echo "  unit_test        to run the unit tests"
	@echo "  integration_test to run the integration tests"
	@echo "  example_test     to run the example tests"
	@echo "  test             to run all tests (unit, integration, example)"
	@echo "  hlint            to run HLint"
	@echo "  hlint_refactor   to have HLint automatically refactor hints"
	@echo "  test_sdist       to build an sdist package and test it"

##############################################################################
# Testing
##############################################################################
INTEGRATION_DIR:=./integration-test/
EXAMPLE_DIR:=./example/
TEST:=stack build \
	--copy-bins --install-ghc \
	--test --no-terminal \
	--haddock --no-haddock-deps

.PHONY: unit_test
INSTALL:=stack build --install-ghc --copy-bins --no-terminal
TEST:=stack build --test --no-terminal --haddock --no-haddock-deps
unit_test:
	$(INSTALL) && $(TEST) tasty-discover:unit-tests

.PHONY: integration_test
integration_test:
	cd $(INTEGRATION_DIR) && \
	$(TEST) tasty-discover-integration-test:configurable-suffix && \
	$(TEST) tasty-discover-integration-test:no-module-suffix

.PHONY: example_test
example_test:
	cd $(EXAMPLE_DIR) && \
	$(TEST) example:example-test

.PHONY: test
test: unit_test integration_test example_test

##############################################################################
# Code Quality
##############################################################################
hlint-install:
	stack install --install-ghc hlint

hlint-apply-refact: hlint-install
	stack install --install-ghc apply-refact

.PHONY: hlint
hlint: hlint-install
	hlint src/ test/

.PHONY: hlint_refactor
HLINT=hlint --refactor --refactor-options -i {} \;
hlint_refactor: hlint-apply-refact
	find src/ test/ -name "*.hs" -exec $(HLINT)

##############################################################################
# Documentation
##############################################################################
SPHINXBUILD:=sphinx-build
BUILDDIR:=docs/build

.PHONY: clean
clean:
	rm -rf $(BUILDDIR)/*

.PHONY: html
html:
	$(SPHINXBUILD) -b html docs/ $(BUILDDIR)/html

.PHONY: live_html
live_html:
	$(SPHINXBUILD) -b html docs/ $(BUILDDIR)/html

##############################################################################
# Source distribution
##############################################################################
sdist:
	stack install --install-ghc && stack sdist

DIST_DIR:=$$(stack path --dist-dir)
SDIST_TAR:=$$(find $(DIST_DIR) -name "*.tar.gz" | tail -1)
untar-sdist: sdist
	tar xzf $(SDIST_TAR)

SDIST_FOLDER:=$$(basename $(SDIST_TAR) .tar.gz)
INIT:=$$(stack init --force)
test_sdist: untar-sdist
	cd $(SDIST_FOLDER) && $(INIT) && $(TEST)
