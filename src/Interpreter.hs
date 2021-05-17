module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map as Map
import Data.Maybe
import Env
import Eval
import PythonScript.Abs
import Types (Context, MemVal (BoolVal, IntVal), MyEnv, ReturnResult)
import Variables (declareVariables)

returnNothing :: Context MyEnv
returnNothing = ask

-- helper function
execFor :: Expr -> Stmt -> Stmt -> Context ()
execFor cond inc code = do
  BoolVal res <- evalExpression cond
  if res
    then do
      execStmt code
      execStmt inc
      execFor cond inc code
    else pure ()

execStmt :: Stmt -> Context MyEnv
execStmt (BStmt (Block stmts)) = do
  env <- ask
  runStatemets stmts
  return env
-- print
execStmt (Print es) = do
  str <- evalExpressions es
  liftIO $ putStrLn $ intercalate ", " $ Data.List.map show str
  returnNothing

-- variables
execStmt (Decl t items) = do
  declareVariables t items
execStmt (Ass ident e) = do
  res <- evalExpression e
  env <- ask
  setMem ident res
  returnNothing
execStmt (Incr ident) = do
  IntVal val <- getMem ident
  setMem ident $ IntVal $ val + 1
  returnNothing
execStmt (Decr ident) = do
  IntVal val <- getMem ident
  setMem ident $ IntVal $ val - 1
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

-- while
execStmt (While e code) = do
  env <- ask
  BoolVal res <- evalExpression e
  if res
    then do
      execStmt code
      execStmt (While e code)
    else return env

-- for loop
execStmt (ForLoop init cond inc code) = do
  prvs_env <- ask
  env <- execStmt init
  local (const env) $ execFor cond inc code
  return prvs_env

-- empty
execStmt Empty = returnNothing

runStatemets :: [Stmt] -> Context MyEnv
runStatemets [] = ask
runStatemets (stmt : rest) = do
  env <- execStmt stmt
  local (const env) $ runStatemets rest

runProgram prog = runExceptT $ runStateT (runReaderT (runStatemets prog) Map.empty) (Map.empty, 0)
