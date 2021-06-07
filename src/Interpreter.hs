module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map as Map
import Data.Maybe
import Env
import PythonScript.Abs
import Types

returnNothing :: Context (MyEnv, ReturnResult)
returnNothing = do
  env <- ask
  return (env, Nothing)

-- variables

defaultVal :: Type -> MemVal
defaultVal Int = IntVal 0
defaultVal Str = StringVal ""
defaultVal Bool = BoolVal False
defaultVal Char = CharVal '\0'

declareVariable :: Type -> MyEnv -> Item -> Context MyEnv
declareVariable t env (Init name e) = do
  val <- evalExpression e
  newMem name val env
declareVariable t env (NoInit name) =
  newMem name (defaultVal t) env

declareVariables :: Type -> [Item] -> Context MyEnv
declareVariables t items = do
  env <- ask
  foldM (declareVariable t) env items

-- evaluating expresssions

evalAddOp Minus x y = x - y
evalAddOp Plus x y = x + y

evalRelOp GTH x y = x > y
evalRelOp GE x y = x >= y
evalRelOp LTH x y = x < y
evalRelOp EQU x y = x == y
evalRelOp NE x y = x /= y
evalRelOp LE x y = x <= y

evalExpression :: Expr -> Context MemVal
-- math
evalExpression (EAdd x op y) = do
  IntVal r1 <- evalExpression x
  IntVal r2 <- evalExpression y
  return $ IntVal $ evalAddOp op r1 r2
evalExpression (Neg e) = do
  IntVal res <- evalExpression e
  return $ IntVal $ - res
-- logical operation
evalExpression (ERel a op b) = do
  r1 <- evalExpression a
  r2 <- evalExpression b
  return $ BoolVal $ evalRelOp op r1 r2

--literals
evalExpression (EString s) = return $ StringVal s
evalExpression (ELitInt i) = return $ IntVal i
evalExpression ELitTrue = return $ BoolVal True
evalExpression ELitFalse = return $ BoolVal False
evalExpression (ELitChar c) = return $ CharVal c
-- variables
evalExpression (EVar ident) = getMem ident

evalExpressions :: [Expr] -> Context [MemVal]
evalExpressions = mapM evalExpression

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

argToFuncArg :: Arg -> FunArg
argToFuncArg (Arg (Reference t) ident) = (ident, t, ByRef)
argToFuncArg (Arg (PythonScript.Abs.ByValue t) ident) = (ident, t, Types.ByValue)

execStmt :: Stmt -> Context (MyEnv, ReturnResult)
execStmt (BStmt (Block stmts)) = do
  env <- ask
  (_, ret) <- runStatemets stmts
  return (env, ret)
-- print
execStmt (Print es) = do
  str <- evalExpressions es
  liftIO $ putStrLn $ intercalate ", " $ Data.List.map show str
  returnNothing

-- variables
execStmt (Decl t items) = do
  env <- declareVariables t items
  return (env, Nothing)
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
    else return (env, Nothing)

-- for loop
execStmt (ForLoop init cond inc code) = do
  prvs_env <- ask
  env <- execStmt init
  execFor cond inc code
  return (prvs_env, Nothing)

-- functions

execStmt (FnDef t ident args code) = do
  env <- ask
  newMem ident (FuncVal (code, Data.List.map argToFuncArg args, t)) env
  returnNothing
execStmt (Ret expr) = do
  res <- evalExpression expr
  env <- ask
  return (env, Just res)
execStmt VRet = do
  env <- ask
  return (env, Just VoidVal)
-- tuples

-- generators

-- empty
execStmt Empty = returnNothing

runStatemets :: [Stmt] -> Context (MyEnv, ReturnResult)
runStatemets [] = returnNothing
runStatemets (stmt : rest) = do
  (env, ret) <- execStmt stmt
  if isNothing ret
    then local (const env) $ runStatemets rest
    else return (env, ret)

runProgram prog = runExceptT $ runStateT (runReaderT (runStatemets prog) Map.empty) (Map.empty, 0)
