module Test.Tasty.Config (
    Config
  , parseConfig
  , usage
  , defaultConfig
) where

import System.Console.GetOpt (
    getOpt
  , OptDescr(Option)
  , ArgDescr(ReqArg)
  , ArgOrder(Permute)
  )

data Config = Config {
  configModuleName :: Maybe String
} deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config Nothing

options :: [OptDescr (Config -> Config)]
options = [Option [] ["module-name"] (ReqArg (\s c -> c {configModuleName = Just s}) "NAME") ""]

usage :: String -> String
usage prog = "\nUsage: " ++ prog ++ " SRC CUR DST [--module-name=NAME]\n"

parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) -> Right $ foldl (flip id) defaultConfig opts
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "'\n")
  where
    formatError err = Left (prog ++ ": " ++ err ++ usage prog)
