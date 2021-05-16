module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map as Map
import Data.Maybe
import Eval
import PythonScript.Abs
import Types (Context, MyEnv, ReturnResult)

returnNothing :: Context (MyEnv, ReturnResult)
returnNothing = do
  env <- ask
  return (env, Nothing)

execStmt :: Stmt -> Context (MyEnv, ReturnResult)
execStmt (Print es) = do
  str <- evalExpressions es
  liftIO $ putStr $ intercalate ", " $ Data.List.map show str
  returnNothing

runStatemets :: [Stmt] -> Context (MyEnv, ReturnResult)
runStatemets [] = returnNothing
runStatemets (stmt : rest) = do
  execStmt stmt
  runStatemets rest

runProgram prog = runExceptT $ runStateT (runReaderT (runStatemets prog) Map.empty) (Map.empty, 0)
