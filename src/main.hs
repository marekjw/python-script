module Main where

import Control.Exception (IOException, catch)
import Control.Monad (when)
import Interpreter
import PythonScript.Abs (Program (Program))
import PythonScript.Lex (Token)
import PythonScript.Par (myLexer, pProgram)
import PythonScript.Print (Print, printTree)
import PythonScript.Skel ()
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (getContents, hGetContents, hPutStr, hPutStrLn, print, stderr, stdin)
import TypeCheck
import Prelude
  ( Either (..),
    FilePath,
    IO,
    Int,
    Show,
    String,
    getContents,
    mapM_,
    putStrLn,
    readFile,
    show,
    unlines,
    ($),
    (++),
    (>),
    (>>),
    (>>=),
  )

type Err = Either String

type ParseFun a = [Token] -> Err a

run :: ParseFun Program -> String -> IO ()
run p s =
  case p ts of
    Left err -> do
      putStrLn err
      exitFailure
    Right (Program statements) -> do
      typeCheckRes <- checkProgram statements
      case typeCheckRes of
        Left err -> do
          print err
          exitFailure
        Right _ -> do
          result <- runProgram statements
          case result of
            Right _ -> exitSuccess
            Left err -> print err
  where
    ts = myLexer s

main :: IO ()
main = do
  getContents >>= run pProgram
