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
import System.IO (getContents, hGetContents, hPutStr, hPutStrLn, stderr, stdin)
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

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: Verbosity -> ParseFun Program -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: Verbosity -> ParseFun Program -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      putStrV v $ show ts
      putStrLn err
      exitFailure
    Right (Program statements) -> do
      runProgram statements
      exitSuccess
  where
    ts = myLexer s

usage :: IO ()
usage = do
  putStrLn $
    unlines
      [ "usage: Call with one of the following argument combinations:",
        "  --help          Display this help message.",
        "  (no arguments)  Parse stdin verbosely.",
        "  (files)         Parse content of files verbosely.",
        "  -s (files)      Silent mode. Parse content of files silently."
      ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pProgram
    "-s" : fs -> mapM_ (runFile 0 pProgram) fs
    fs -> mapM_ (runFile 2 pProgram) fs
