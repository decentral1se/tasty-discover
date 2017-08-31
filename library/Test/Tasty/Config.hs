-- | The test driver configuration options module.
--
-- Anything that can be passed as an argument to the test driver
-- definition exists as a field in the 'Config' type.

module Test.Tasty.Config (
  -- * Configuration Options
  Config (..)
  , GlobPattern

  -- * Configuration Parser
  , parseConfig

  -- * Configuration Defaults
  , defaultConfig
  ) where

import           Data.Maybe            (isJust)
import           System.Console.GetOpt (ArgDescr (NoArg, ReqArg),
                                        ArgOrder (Permute), OptDescr (Option),
                                        getOpt)

-- | A tasty ingredient.
type Ingredient = String

-- | A glob pattern.
type GlobPattern = String

-- | The discovery and runner configuration.
data Config = Config
  { modules             :: Maybe GlobPattern -- ^ Glob pattern for matching modules during test discovery.
  , moduleSuffix        :: Maybe String      -- ^ <<<DEPRECATED>>>: Module suffix.
  , generatedModuleName :: Maybe String      -- ^ Name of the generated main module.
  , ignores             :: Maybe GlobPattern -- ^ Glob pattern for ignoring modules during test discovery.
  , ignoredModules      :: [FilePath]        -- ^ <<<DEPRECATED>>>: Ignored modules by full name.
  , tastyIngredients    :: [Ingredient]      -- ^ Tasty ingredients to use.
  , noModuleSuffix      :: Bool              -- ^ <<<DEPRECATED>>>: suffix and look in all modules.
  , debug               :: Bool              -- ^ Debug the generated module.
  , treeDisplay         :: Bool              -- ^ Tree display for the test results table.
  } deriving (Show)

-- | The default configuration
defaultConfig :: Config
defaultConfig = Config Nothing Nothing Nothing Nothing [] [] False False False

-- | Deprecation message for old `--[no-]module-suffix` option.
moduleSuffixDeprecationMessage :: String
moduleSuffixDeprecationMessage =
  error $ concat
    [ "\n\n"
    , "----------------------------------------------------------\n"
    , "DEPRECATION NOTICE: `--[no-]module-suffix` is deprecated.\n"
    , "The default behaviour now discovers all test module suffixes.\n"
    , "Please use the `--modules='<glob-pattern>'` option to specify.\n"
    , "----------------------------------------------------------\n"
    ]

-- | Deprecation message for old `--ignore-module` option.
ignoreModuleDeprecationMessage :: String
ignoreModuleDeprecationMessage =
  error $ concat
    [ "\n\n"
    , "----------------------------------------------------------\n"
    , "DEPRECATION NOTICE: `--ignore-module` is deprecated.\n"
    , "Please use the `--ignores='<glob-pattern>'` option instead.\n"
    , "----------------------------------------------------------\n"
    ]

-- | Configuration options parser.
parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) ->
      let config = foldl (flip id) defaultConfig opts in
        if noModuleSuffix config || isJust (moduleSuffix config) then
          error moduleSuffixDeprecationMessage
        else
        if not $ null (ignoredModules config) then
          error ignoreModuleDeprecationMessage
        else
        Right config
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "`\n")
  where
    formatError err = Left (prog ++ ": " ++ err)

-- | All configuration options.
options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["modules"]
      (ReqArg (\s c -> c {modules = Just s}) "GLOB-PATTERN")
      "Specify desired modules with a glob pattern (white-list)"
  , Option [] ["module-suffix"]
      (ReqArg (\s c -> c {moduleSuffix = Just s}) "SUFFIX")
      "<<<DEPRECATED>>>: Specify desired test module suffix"
  , Option [] ["generated-module"]
      (ReqArg (\s c -> c {generatedModuleName = Just s}) "MODULE")
      "Qualified generated module name"
  , Option [] ["ignores"]
      (ReqArg (\s c -> c {ignores = Just s}) "GLOB-PATTERN")
      "Specify desired modules to ignore with a glob pattern (black-list)"
  , Option [] ["ignore-module"]
      (ReqArg (\s c -> c {ignoredModules = s : ignoredModules c}) "FILE")
      "<<<DEPRECATED>>>: Ignore a test module"
  , Option [] ["ingredient"]
      (ReqArg (\s c -> c {tastyIngredients = s : tastyIngredients c}) "INGREDIENT")
      "Qualified tasty ingredient name"
  , Option [] ["no-module-suffix"]
      (NoArg $ \c -> c {noModuleSuffix = True})
      "<<<DEPRECATED>>>: Ignore test module suffix and import them all"
  , Option [] ["debug"]
      (NoArg $ \c -> c {debug = True})
      "Debug output of generated test module"
  , Option [] ["tree-display"]
      (NoArg $ \c -> c {treeDisplay = True})
      "Display test output hierarchically"
  ]
