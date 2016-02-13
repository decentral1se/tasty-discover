module Run (run) where

-- System
import System.IO
import System.Exit
import System.Environment  (getProgName)

-- Config
import Config              (parseConfig, usage)

run :: [String] -> IO ()
run processor_args = do
  name <- getProgName
  case processor_args of
    src : _ : dst : opts -> case parseConfig name opts of

      Left err -> do
        hPutStrLn stderr err
        exitFailure

      Right conf -> do
        writeFile dst "main = undefined"

    _ -> do
      hPutStrLn stderr (usage name)
      exitFailure
