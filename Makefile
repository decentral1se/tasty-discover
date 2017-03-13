TEST:=stack build --test --haddock --no-haddock-deps

unit_test:
	$(TEST) tasty-discover:test
.PHONY: unit_test

example_test:
	cd example-usage/ && $(TEST) --install-ghc
.PHONY: example_test

hlint-install:
	stack install hlint
.PHONY: hlint-install

hlint: hlint-install
	hlint library/ executable/ test/
.PHONY: hlint

hlint-apply-refact: hlint-install
	stack install apply-refact
.PHONY: hlint-apply-refact

HLINT=hlint --refactor --refactor-options -i {} \;
hlint_refactor: hlint-apply-refact
	find library/ executable/ test/ -name "*.hs" -exec $(HLINT)
.PHONY: hlint_refactor

sdist:
	stack sdist
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
