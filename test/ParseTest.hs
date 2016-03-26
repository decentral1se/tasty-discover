-- Unit tests for Test.Tasty.Parse module.

module ParseTest where

import           Test.Tasty.Discover (Assertion,
                                      Config (Config, configModuleSuffix),
                                      parseConfig, (@?=))

case_parse_config :: Assertion
case_parse_config =
    parseConfig "foo" ["--module-suffix=MySuffix"]
    @?=
    Right Config {configModuleSuffix=Just "MySuffix"}

case_parse_config_missing_arg :: Assertion
case_parse_config_missing_arg =
    parseConfig "foo" ["--module-suffix"]
    @?=
    Left "foo: option `--module-suffix' requires an argument SUFFIX\n"

case_parse_config_empty_arg :: Assertion
case_parse_config_empty_arg =
    parseConfig "foo" []
    @?=
    Right (Config Nothing)

case_parse_config_invalid_arg :: Assertion
case_parse_config_invalid_arg =
    parseConfig "foo" ["a"]
    @?=
    Left "foo: unexpected argument `a`\n"
