-- | Parser for the GHC preprocessor definition.

module Test.Tasty.Discover.Parse (
  parseConfig
) where

import Data.Maybe (isJust)
import System.Console.GetOpt (ArgOrder (Permute), getOpt)

import Test.Tasty.Discover.Config  (Config(..), defaultConfig, options)

-- | Preprocessor configuration parser.
parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) ->
      let config   = foldl (flip id) defaultConfig opts
          errorMsg = "You cannot combine '--no-module-suffix' and '--module-suffix'\n"
      in
        if noModuleSuffix config && isJust (configModuleSuffix config)
        then formatError errorMsg
        else Right config
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "`\n")
  where
    formatError err = Left (prog ++ ": " ++ err)
