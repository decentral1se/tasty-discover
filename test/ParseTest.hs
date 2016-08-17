-- Unit tests for Test.Tasty.Parse module.

module ParseTest where

import           Test.Tasty.Discover (Assertion,
                                      Config (Config, configModuleSuffix),
                                      parseConfig, (@?=))

case_parseConfig :: Assertion
case_parseConfig =
    parseConfig "foo" ["--module-suffix=MySuffix"]
    @?=
    Right Config {configModuleSuffix=Just "MySuffix"}

case_parseConfigMissingArg :: Assertion
case_parseConfigMissingArg =
    parseConfig "foo" ["--module-suffix"]
    @?=
    Left "foo: option `--module-suffix' requires an argument SUFFIX\n"

case_parseConfigEmptyArg :: Assertion
case_parseConfigEmptyArg =
    parseConfig "foo" []
    @?=
    Right (Config Nothing)

case_parseConfigInvalidArg :: Assertion
case_parseConfigInvalidArg =
    parseConfig "foo" ["a"]
    @?=
    Left "foo: unexpected argument `a`\n"
