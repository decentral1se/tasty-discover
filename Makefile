TEST:=stack build --test --haddock --no-haddock-deps
test:
	 $(TEST) tasty-discover:test
.PHONY: test

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

stylish_haskell_install:
	stack install stylish-haskell
.PHONY: stylish-haskell-install

STYLISH=stylish-haskell -i {} \;
stylish: stylish-haskell-install
	find library/ executable/ test/ -name "*.hs" -exec $(STYLISH) && git diff --exit-code
.PHONY: stylish-haskell

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
