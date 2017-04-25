-- Configuration options module.
module Test.Tasty.Config
  ( Config(..)
  , parseConfig
  , defaultConfig
  ) where

import           System.Console.GetOpt (ArgDescr (NoArg, ReqArg),
                                        ArgOrder (Permute), OptDescr (Option),
                                        getOpt)

type Ingredient = String

data Config = Config
  { moduleSuffix        :: Maybe String
  , generatedModuleName :: Maybe String
  , ignoredModules      :: [FilePath]
  , tastyIngredients    :: [Ingredient]
  , noModuleSuffix      :: Bool
  , debug               :: Bool
  } deriving (Show)

-- | The default configuration
defaultConfig :: Config
defaultConfig = Config Nothing Nothing [] [] False False

noModuleSuffixDeprecationMessage :: String
noModuleSuffixDeprecationMessage =
  error $ concat
    [ "\n\n"
    , "-------------------------------------------------------------------\n"
    , "DEPRECATION NOTICE: The `--no-module-suffix` option is deprecated.\n"
    , "Please use the `--module-suffix` with glob syntax.\n"
    , "Please see https://github.com/lwm/tasty-discover/issues/84.\n"
    , "-------------------------------------------------------------------\n"
    ]

-- | Configuration options parser.
parseConfig :: String -> [String] -> Either String Config
parseConfig prog args = case getOpt Permute options args of
    (opts, [], []) ->
      let config = foldl (flip id) defaultConfig opts in
        if noModuleSuffix config
        then error noModuleSuffixDeprecationMessage
        else Right config
    (_, _, err:_)  -> formatError err
    (_, arg:_, _)  -> formatError ("unexpected argument `" ++ arg ++ "`\n")
  where
    formatError err = Left (prog ++ ": " ++ err)

-- | All configuration options.
options :: [OptDescr (Config -> Config)]
options = [
    Option [] ["module-suffix"]
      -- FIXME: This should be called 'modules' instead
      (ReqArg (\s c -> c {moduleSuffix = Just s}) "FILE-GLOB")
      "Specify desired test module suffix via file glob"
  , Option [] ["generated-module"]
      (ReqArg (\s c -> c {generatedModuleName = Just s}) "MODULE")
      "Qualified generated module name"

    -- FIXME: This should be called 'ignores' instead
  , Option [] ["ignore-module"]
      (ReqArg (\s c -> s {ignoredModules = Just s}) "FILE-GLOB")
      "Specify desired test modules to ignore via file glob"

  , Option [] ["ingredient"]
      (ReqArg (\s c -> c {tastyIngredients = s : tastyIngredients c}) "INGREDIENT")
      "Qualified tasty ingredient name"
  , Option [] ["debug"]
      (NoArg $ \c -> c {debug = True})
      "Debug output of generated test module"

  -- DEPRECATED: Use --module-suffix with new glob syntax
  , Option [] ["no-module-suffix"]
      (NoArg $ \c -> c {noModuleSuffix = True})
      "Ignore test module suffix and import them all"
  ]
