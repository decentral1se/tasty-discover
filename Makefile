ROOT_DIR:=.
INTEGRATION_DIR:=./integration-test/
EXAMPLE_DIR:=./tasty-discover-example/

STACK_TEST:=stack test --pedantic

unit_test:
	stack test tasty-discover:unit-tests

integration_test:
	cd $(INTEGRATION_DIR) \
	&& stack test tasty-discover-integration-test:configurable-suffix \
	&& cd -

example_test:
	cd $(EXAMPLE_DIR) \
	&& stack test tasty-discover-example:example-test \
	&& cd -

test:
	$(MAKE) unit_test \
	&& $(MAKE) integration_test \
	&& $(MAKE) example_test

.PHONY: unit_test integration_test example_test test
