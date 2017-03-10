INTEGRATION_DIR:=./integration-test/
EXAMPLE_DIR:=./example/
TEST:=stack build \
	--copy-bins --install-ghc \
	--test --no-terminal \
	--haddock --no-haddock-deps

INSTALL:=stack build --install-ghc --copy-bins --no-terminal
TEST:=stack build --test --no-terminal --haddock --no-haddock-deps
unit_test:
	$(INSTALL) && $(TEST) tasty-discover:unit-tests
.PHONY: unit_test

integration_test:
	cd $(INTEGRATION_DIR) && \
	$(TEST) tasty-discover-integration-test:configurable-suffix && \
	$(TEST) tasty-discover-integration-test:no-module-suffix    && \
	$(TEST) tasty-discover-integration-test:ignore-module
.PHONY: integration_test

example_test:
	cd $(EXAMPLE_DIR) && \
	$(TEST) example:example-test
.PHONY: example_test

test: unit_test integration_test example_test
.PHONY: test

hlint-install:
	stack install --install-ghc hlint
.PHONY: hlint-install

hlint-apply-refact: hlint-install
	stack install --install-ghc apply-refact
.PHONY: hlint-apply-refact

hlint: hlint-install
	hlint src/ test/
.PHONY: hlint

HLINT=hlint --refactor --refactor-options -i {} \;
hlint_refactor: hlint-apply-refact
	find src/ test/ -name "*.hs" -exec $(HLINT)
.PHONY: hlint_refactor

sdist:
	stack install --install-ghc && stack sdist
.PHONY: sdist

DIST_DIR:=$$(stack path --dist-dir)
SDIST_TAR:=$$(find $(DIST_DIR) -name "*.tar.gz" | tail -1)
untar_sdist: sdist
	tar xzf $(SDIST_TAR)
.PHONY: untar_sdist

SDIST_FOLDER:=$$(basename $(SDIST_TAR) .tar.gz)
INIT:=$$(stack init --force)
test_sdist: untar_sdist
	cd $(SDIST_FOLDER) && $(INIT) && $(TEST)
.PHONY: test_sdist
