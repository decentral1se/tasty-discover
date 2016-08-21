-- | Parser for the GHC preprocessor definition.

module Test.Tasty.Parse (
  parseConfig
) where

import System.Console.GetOpt (ArgOrder (Permute), getOpt)

import Test.Tasty.Config  (Config, defaultConfig, options)

-- | Preprocessor configuration parser.
parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) -> Right $ foldl (flip id) defaultConfig opts
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "`\n")
  where
    formatError err = Left (prog ++ ": " ++ err)
