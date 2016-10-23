ROOT_DIR:=.
INTEGRATION_DIR:=./integration-test/
EXAMPLE_DIR:=./example/

STACK_TEST:=stack test --pedantic

install_and_cover:
	stack install --coverage

unit_test:
	$(STACK_TEST) tasty-discover:unit-tests

integration_test:
	cd $(INTEGRATION_DIR) \
	&& $(STACK_TEST) tasty-discover-integration-test:configurable-suffix \
	&& $(STACK_TEST) tasty-discover-integration-test:no-module-suffix \
	&& cd -

example_test:
	cd $(EXAMPLE_DIR) \
	&& $(STACK_TEST) example:example-test \
	&& cd -

test: unit_test integration_test example_test

.PHONY: unit_test integration_test example_test test
