module Test.Tasty.Prelude (
      getOpt
    , (</>)
    , (<|>)
    , ArgDescr(ReqArg)
    , ArgOrder(Permute)
    , IsString
    , OptDescr(Option)
    , doesDirectoryExist
    , doesFileExist
    , exitFailure
    , extractTestFunctions
    , filterM
    , fromString
    , getArgs
    , getDirectoryContents
    , getProgName
    , hPutStrLn
    , intercalate
    , isAlphaNum
    , isUpper
    , mapMaybe
    , sort
    , splitDirectories
    , splitFileName
    , stderr
    , stripPrefix
) where

import           System.Console.GetOpt (ArgDescr (ReqArg), ArgOrder (Permute),
                                        OptDescr (Option), getOpt)
import           System.Directory      (doesDirectoryExist, doesFileExist,
                                        getDirectoryContents)
import           System.Environment    (getArgs, getProgName)
import           System.Exit           (exitFailure)
import           System.FilePath       (splitDirectories, splitFileName, (</>))
import           System.IO             (hPutStrLn, stderr)

import           Data.Char             (isAlphaNum, isUpper)
import           Data.List             (intercalate, sort, stripPrefix)
import           Data.Maybe            (mapMaybe)
import           Data.String           (IsString, fromString)

import           Control.Applicative   ((<|>))
import           Control.Monad         (filterM)
import           Test.Tasty.TH         (extractTestFunctions)
