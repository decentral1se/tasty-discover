-- | The test driver configuration options module.
--
-- Anything that can be passed as an argument to the test driver
-- definition exists as a field in the 'Config' type.

module Test.Tasty.Config (
  -- * Configuration Options
  Config (..)

  -- * Configuration Parser
  , parseConfig

  -- * Configuration Defaults
  , defaultConfig
  ) where

import           Data.Maybe            (isJust)
import           System.Console.GetOpt (ArgDescr (NoArg, ReqArg),
                                        ArgOrder (Permute), OptDescr (Option),
                                        getOpt)

-- | A type alias for readability.
type Ingredient = String

data Config = Config
  { moduleSuffix        :: Maybe String -- ^ Module suffix.
  , generatedModuleName :: Maybe String -- ^ Name of the generated main module.
  , ignoredModules      :: [FilePath]   -- ^ Ignored modules by full name.
  , tastyIngredients    :: [Ingredient] -- ^ Tasty ingredients to use.
  , noModuleSuffix      :: Bool         -- ^ suffix and look in all modules.
  , debug               :: Bool         -- ^ Debug the generated module.
  , treeDisplay         :: Bool         -- ^ Tree display for the test results table.
  } deriving (Show)

-- | The default configuration
defaultConfig :: Config
defaultConfig = Config Nothing Nothing [] [] False False False

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
  , Option [] ["tree-display"]
      (NoArg $ \c -> c {treeDisplay = True})
      "Display test output hierarchically"
  ]
