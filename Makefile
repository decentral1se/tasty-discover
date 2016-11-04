# Stack related
ROOT_DIR:=.
INTEGRATION_DIR:=./integration-test/
EXAMPLE_DIR:=./example/
STACK_TEST:=stack test --no-terminal --haddock --no-haddock-deps

# Sphinx related
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =
BUILDDIR      = build
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d $(BUILDDIR)/doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) source
I18NSPHINXOPTS  = $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) source

.PHONY: help
help:
	@echo "Please use \`make <target>' where <target> is one of"
	@echo "  html             to make standalone HTML files"
	@echo "  changes          to make an overview of all changed/added/deprecated items"
	@echo "  live_html        to interactively build/change the documentation"
	@echo "  dummy            to check syntax errors of document sources"
	@echo "  unit_test        to run the unit tests"
	@echo "  integration_test to run the integration tests"
	@echo "  example_test     to run the example tests"
	@echo "  test             to run all tests (unit, integration, example)"
	@echo "  hlint            to run HLint"
	@echo "  hlint_refactor   to have HLint automatically refactor hints"
	@echo "  haddock          to build the haddock documentation"

.PHONY: unit_test
unit_test:
	$(STACK_TEST) tasty-discover:unit-tests

.PHONY: integration_test
integration_test:
	cd $(INTEGRATION_DIR) \
	&& $(STACK_TEST) tasty-discover-integration-test:configurable-suffix \
	&& $(STACK_TEST) tasty-discover-integration-test:no-module-suffix \
	&& cd -

.PHONY: haddock
haddock:
	stack haddock

.PHONY: example_test
example_test:
	cd $(EXAMPLE_DIR) \
	&& $(STACK_TEST) example:example-test \
	&& cd -

.PHONY: test
test: unit_test integration_test example_test

.PHONY: hlint
hlint:
	hlint src/ test/
	@echo
	@echo "HLint hint check finished. You can use 'make hlint_refactor'" \
	      "to have HLint automatically refactor those hints"

.PHONY: hlint_refactor
hlint_refactor:
	find src/ test/ -name "*.hs" \
	-exec hlint --refactor --refactor-options -i {} \;
	@echo
	@echo "HLint refactor finished. Run 'git status' to review changes."

.PHONY: clean
clean:
	rm -rf $(BUILDDIR)/*
	@echo
	@echo "Clean finished. $(BUILDDIR) has been removed."

.PHONY: html
html:
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html
	@echo
	@echo "Build finished. The HTML pages are in $(BUILDDIR)/html."

.PHONY: linkcheck
linkcheck:
	$(SPHINXBUILD) -b linkcheck $(ALLSPHINXOPTS) $(BUILDDIR)/linkcheck
	@echo
	@echo "Link check complete; look for any errors in the above output " \
	      "or in $(BUILDDIR)/linkcheck/output.txt."

.PHONY: live_html
live_html:
	sphinx-autobuild -b html $(ALLSPHINXOPTS) $(BUILDDIR)/html
