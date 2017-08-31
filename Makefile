TEST:=stack build --test --haddock --no-haddock-deps --pedantic
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
test_sdist: untar_sdist
	cd $(SDIST_FOLDER) && $(TEST)
.PHONY: test_sdist

stylish_haskell_install:
	stack install stylish-haskell
.PHONY: stylish_haskell_install

STYLISH=stylish-haskell -i {} \;
stylish_haskell: stylish_haskell_install
	find library/ executable/ test/ -name "*.hs" -exec $(STYLISH) && git diff --exit-code
.PHONY: stylish_haskell

hlint_install:
	stack install hlint
.PHONY: hlint_install

hlint: hlint_install
	hlint --cpp-simple library/ executable/ test/
.PHONY: hlint

hlint_apply_refact: hlint_install
	stack install apply-refact
.PHONY: hlint_apply_refact

HLINT=hlint --cpp-simple --refactor --refactor-options -i {} \;
hlint_refactor: hlint_apply_refact
	find library/ executable/ test/ -name "*.hs" -exec $(HLINT)
.PHONY: hlint_refactor

upload:
	stack upload .
.PHONY: upload

test_readme_dependencies:
	stack install tasty-hunit tasty-hspec tasty-quickcheck tasty markdown-unlit
.PHONY: test_readme_dependencies

test_readme: test_readme_dependencies
	stack exec -- ghc -pgmL markdown-unlit README.lhs
.PHONY: test_readme
