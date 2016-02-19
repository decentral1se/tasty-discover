### tasty-discover example

This is a minimal project setup to demonstrate how simple it is to setup a test
suite with `tasty-discover`.

### Quick start

```
$ git clone https://github.com/<your-username>/tasty-discover
$ cd tasty-discover/integration-test
$ stack setup && stack test && cat .stack-work/logs/*-test.log
```

As you can see from the example code, the major steps are:

  - define a test suite main in your cabal file
  - add the preprocessor line to your main file

Then simply write your tests as you want in whatever you location you want (as
long as it is under `hs-source-dirs`) and `tasty-discover` generates all the
boilerplate for you!

[rootreadme]: https://github.com/lwm/tasty-discover/blob/master/README.md
