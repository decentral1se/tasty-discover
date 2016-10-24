ROOT_DIR:=.
INTEGRATION_DIR:=./integration-test/
EXAMPLE_DIR:=./example/

STACK_TEST:=stack test --pedantic

unit_test:
	$(STACK_TEST) tasty-discover:unit-tests

doctest:
	stack test tasty-discover:doctest

integration_test:
	cd $(INTEGRATION_DIR) \
	&& $(STACK_TEST) tasty-discover-integration-test:configurable-suffix \
	&& $(STACK_TEST) tasty-discover-integration-test:no-module-suffix \
	&& cd -

example_test:
	cd $(EXAMPLE_DIR) \
	&& $(STACK_TEST) example:example-test \
	&& cd -

test: unit_test integration_test example_test doctest

hlint:
	hlint src/ test/

hlint_refactor:
	find src/ test/ -name "*.hs" \
	-exec hlint --refactor --refactor-options -i {} \;

.PHONY: unit_test integration_test example_test test hlint hlint_refactor doctest
