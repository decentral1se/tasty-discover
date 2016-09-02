-- Unit tests for Test.Tasty.Parse module.

module ParseTest where

import Test.Tasty.Discover (parseConfig, Config(..),
                            Assertion, (@?=))

case_parseConfig :: Assertion
case_parseConfig =
    parseConfig "foo" ["--module-suffix=MySuffix"]
    @?=
    Right Config {configModuleSuffix=Just "MySuffix" , noModuleSuffix=False}

case_parseConfigMissingArg :: Assertion
case_parseConfigMissingArg =
    parseConfig "foo" ["--module-suffix"]
    @?=
    Left "foo: option `--module-suffix' requires an argument SUFFIX\n"

case_parseConfigEmptyArg :: Assertion
case_parseConfigEmptyArg =
    parseConfig "foo" []
    @?=
    Right (Config Nothing False)

case_parseConfigInvalidArg :: Assertion
case_parseConfigInvalidArg =
    parseConfig "foo" ["a"]
    @?=
    Left "foo: unexpected argument `a`\n"

case_parseConfigBooleanArg :: Assertion
case_parseConfigBooleanArg  =
    parseConfig "foo" ["--no-module-suffix"]
    @?=
    Right Config {configModuleSuffix=Nothing, noModuleSuffix=True}

case_parseConfigInvalidArgCombination :: Assertion
case_parseConfigInvalidArgCombination =
    parseConfig "foo" ["--module-suffix=MySuffix", "--no-module-suffix"]
    @?=
    Left "foo: You cannot combine '--no-module-suffix' and '--module-suffix'\n"
