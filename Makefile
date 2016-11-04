ROOT_DIR:=.
INTEGRATION_DIR:=./integration-test/
EXAMPLE_DIR:=./example/
STACK_TEST:=stack test --no-terminal --haddock --no-haddock-deps
SPHINXBUILD = sphinx-build
BUILDDIR = docs/build

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

.PHONY: unit_test
unit_test:
	$(STACK_TEST) tasty-discover:unit-tests

.PHONY: integration_test
integration_test:
	cd $(INTEGRATION_DIR) \
	&& $(STACK_TEST) tasty-discover-integration-test:configurable-suffix \
	&& $(STACK_TEST) tasty-discover-integration-test:no-module-suffix \
	&& cd -

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

.PHONY: hlint_refactor
hlint_refactor:
	find src/ test/ -name "*.hs" \
	-exec hlint --refactor --refactor-options -i {} \;

.PHONY: clean
clean:
	rm -rf $(BUILDDIR)/*

.PHONY: html
html:
	$(SPHINXBUILD) -b html $(BUILDDIR)/html

.PHONY: live_html
live_html:
	$(SPHINXBUILD) -b html $(BUILDDIR)/html
