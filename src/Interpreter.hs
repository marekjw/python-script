module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map as Map
import Data.Maybe
import Eval
import PythonScript.Abs
import Types (Context, MemVal (BoolVal, IntVal), MyEnv, ReturnResult)
import Variables (declareVariables)

returnNothing :: Context (MyEnv, ReturnResult)
returnNothing = do
  env <- ask
  return (env, Nothing)

execStmt :: Stmt -> Context (MyEnv, ReturnResult)
execStmt (BStmt (Block stmts)) = runStatemets stmts
-- print
execStmt (Print es) = do
  str <- evalExpressions es
  liftIO $ putStrLn $ intercalate ", " $ Data.List.map show str
  returnNothing

-- variables
execStmt (Decl t items) = do
  env <- declareVariables t items
  returnNothing
-- if
execStmt (Cond cond stmt) = do
  BoolVal res <- evalExpression cond
  if res
    then execStmt stmt
    else returnNothing
execStmt (CondElse cond a b) = do
  BoolVal res <- evalExpression cond
  if res
    then execStmt a
    else execStmt b

-- empty
execStmt Empty = returnNothing

runStatemets :: [Stmt] -> Context (MyEnv, ReturnResult)
runStatemets [] = returnNothing
runStatemets (stmt : rest) = do
  execStmt stmt
  runStatemets rest

runProgram prog = runExceptT $ runStateT (runReaderT (runStatemets prog) Map.empty) (Map.empty, 0)
