module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Hjq (hjq)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode (..))

main :: IO ()
main = do
  args <- getArgs
  case args of
    (query : file : []) -> do
      json <- B.readFile file
      printResult $ hjq json (T.pack query)
    (query : []) -> do
      json <- B.getContents
      printResult $ hjq json (T.pack query)
    _ -> do
      putStrLn $ "Invalid arguments error. : " ++ show args
      exitWith $ ExitFailure 1

printResult :: Either T.Text B.ByteString -> IO ()
printResult (Left err) = do
  T.putStrLn err
  exitWith $ ExitFailure 1
printResult (Right result) = B.putStrLn result
