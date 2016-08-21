-- | Preprocessor configuration.

module Test.Tasty.Config (
  Config(Config)
, defaultConfig
, configModuleSuffix
, options
) where

import System.Console.GetOpt (ArgDescr (ReqArg), OptDescr (Option))

import Test.Tasty.Type (Config (Config), Config (configModuleSuffix))

-- | An empty configuration.
defaultConfig :: Config
defaultConfig = Config Nothing

-- | @TODO
options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["module-suffix"]
        (ReqArg (\s c -> c {configModuleSuffix = Just s}) "SUFFIX") ""
  ]
