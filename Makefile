ROOT_DIR:=.
INTEGRATION_DIR:=./integration-test/
EXAMPLE_DIR:=./tasty-discover-example/

TEST:=stack build --no-terminal --test --pedantic

unit_test:
	cd $(ROOT_DIR) && $(TEST) && cd -

integration_test:
	cd $(INTEGRATION_DIR) && $(TEST) && cd -

example_test:
	cd $(EXAMPLE_DIR) && $(TEST) && cd -

test:
	$(MAKE) unit_test \
  && $(MAKE) integration_test \
  && $(MAKE) example_test

.PHONY: unit_test integration_test example_test test
