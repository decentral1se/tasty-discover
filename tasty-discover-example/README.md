### tasty-discover example

This is a minimal project setup to demonstrate how simple it is to setup a test
suite with `tasty-discover`.

### Quick start

```
$ git clone https://github.com/<your-username>/tasty-discover
$ cd tasty-discover/tasty-discover-example
$ stack setup && stack test
```

You'll get the usual tasty test report:

```
tasty-discover-integration-test-0.0.1: test (suite: tasty-travis-test)

Discovered tests
  nine is nine:            OK
    +++ OK, passed 100 tests.
  the answer:              OK
  the number of the beast: OK
  two is two:              OK
    +++ OK, passed 100 tests.
  three is three:          OK
    +++ OK, passed 100 tests.

All 5 tests passed (0.00s)

Completed 3 action(s).
```

As you can see from the example code, the major steps are:

  - Define a test suite main in your cabal file
  - Add the preprocessor line to your main file
  - End your test file names with `Test.hs`
  - Name your tests `prop_*` or `case_*`

[rootreadme]: https://github.com/lwm/tasty-discover/blob/master/README.md
