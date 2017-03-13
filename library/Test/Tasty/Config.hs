-- Configuration options module.
module Test.Tasty.Config (parseConfig, defaultConfig) where

import System.Console.GetOpt (ArgDescr(ReqArg, NoArg) , OptDescr(Option),
                              ArgOrder(Permute), getOpt)
import Data.Maybe (isJust)
import Test.Tasty.Type (Config(..))

-- | The default configuration
defaultConfig :: Config
defaultConfig = Config Nothing Nothing [] [] False False

-- | Configuration options parser.
parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) ->
      let config   = foldl (flip id) defaultConfig opts
          errorMsg = "You cannot combine '--no-module-suffix' and '--module-suffix'\n"
      in
        if noModuleSuffix config && isJust (moduleSuffix config)
        then formatError errorMsg
        else Right config
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "`\n")
  where
    formatError err = Left (prog ++ ": " ++ err)

-- | All configuration options.
options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["module-suffix"]
      (ReqArg (\s c -> c {moduleSuffix = Just s}) "SUFFIX")
      "Specify desired test module suffix"
  , Option [] ["generated-module"]
      (ReqArg (\s c -> c {generatedModuleName = Just s}) "MODULE")
      "Qualified generated module name"
  , Option [] ["ignore-module"]
      (ReqArg (\s c -> c {ignoredModules = s : ignoredModules c}) "FILE")
      "Ignore a test module"
  , Option [] ["ingredient"]
      (ReqArg (\s c -> c {tastyIngredients = s : tastyIngredients c}) "INGREDIENT")
      "Qualified tasty ingredient name"
  , Option [] ["no-module-suffix"]
      (NoArg $ \c -> c {noModuleSuffix = True})
      "Ignore test module suffix and import them all"
  , Option [] ["debug"]
      (NoArg $ \c -> c {debug = True})
      "Debug output of generated test module"
  ]
