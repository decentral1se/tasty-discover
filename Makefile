EXAMPLE_DIR:=example-usage/
TEST:=stack build \
	--install-ghc --test \
  --no-terminal \
	--haddock --no-haddock-deps

INSTALL:=stack build --install-ghc --copy-bins --no-terminal
TEST:=stack build --test --haddock --no-haddock-deps
unit_test:
	$(INSTALL) && $(TEST) tasty-discover:test
.PHONY: unit_test

example_test:
	cd $(EXAMPLE_DIR) && $(TEST)
.PHONY: example_test

test: unit_test example_test
.PHONY: test

hlint-install:
	stack install --install-ghc hlint
.PHONY: hlint-install

hlint: hlint-install
	hlint library/ executable/ test/
.PHONY: hlint

hlint-apply-refact: hlint-install
	stack install --install-ghc apply-refact
.PHONY: hlint-apply-refact

HLINT=hlint --refactor --refactor-options -i {} \;
hlint_refactor: hlint-apply-refact
	find library/ executable/ test/ -name "*.hs" -exec $(HLINT)
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
