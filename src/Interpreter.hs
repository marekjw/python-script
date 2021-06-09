module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map as Map
import Data.Maybe
import Env
import PythonScript.Abs
import Tuples
import Types

-- helper function

returnNothing :: Context (MyEnv, ReturnResult)
returnNothing = do
  env <- ask
  return (env, Nothing)

parseFuncArgs :: FunArgList -> [Expr] -> MyEnv -> Context MyEnv
parseFuncArgs ((ident, t, passType) : restFunArgs) (expr : restExpr) acc_env =
  case passType of
    Types.ByValue -> do
      val <- evalExpression expr
      new_env <- newMem ident val acc_env
      parseFuncArgs restFunArgs restExpr new_env
    Types.ByRef ->
      case expr of
        EVar ref_ident -> do
          new_env <- rewireMem ref_ident ident acc_env
          parseFuncArgs restFunArgs restExpr new_env
        _ -> throwError $ WrongArgument "Cannot pass value by reference"
parseFuncArgs [] [] acc = return acc
parseFuncArgs _ _ _ = throwError InvalidArgumentCount

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
evalExpression (EMul expr1 op expr2) = do
  IntVal r1 <- evalExpression expr1
  IntVal r2 <- evalExpression expr2
  case op of
    Times -> return $ IntVal $ r1 * r2
    Div ->
      if r2 == 0
        then throwError DivisionByZeroException
        else return $ IntVal $ div r1 r2
    Mod ->
      if r2 == 0
        then throwError DivisionByZeroException
        else return $ IntVal $ mod r1 r2

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
-- call function

evalExpression (EApp ident args) = do
  FuncVal (Block code, f_args, t, f_env) <- getMem ident
  env <- ask
  func_ready_env <- parseFuncArgs f_args args f_env
  local (const func_ready_env) $ do
    (_, res) <- runStatemets code
    case res of
      Nothing ->
        if t == Void
          then return VoidVal
          else throwError NoReturnException
      Just r -> return r

-- lambda function

evalExpression (LambdaFunVal args code) = do
  env <- ask
  case code of
    (BStmt block) -> do
      return $ FuncVal (block, Data.List.map argToFuncArg args, Void, env)
    _ -> do
      return $ FuncVal (Block [code], Data.List.map argToFuncArg args, Void, env)

-- tuples expressions
evalExpression (ETuple exprs) = do
  res <- evalExpressions exprs
  return $ TupleVal res

evalExpressions :: [Expr] -> Context [MemVal]
evalExpressions = mapM evalExpression

-- helper function
execFor :: Expr -> Stmt -> Stmt -> Context (MyEnv, ReturnResult)
execFor cond inc code = do
  BoolVal res <- evalExpression cond
  if res
    then do
      (env, res) <- execStmt code
      if isNothing res
        then do
          execStmt inc
          execFor cond inc code
        else return (env, res)
    else returnNothing

argToFuncArg :: Arg -> FunArg
argToFuncArg (Arg (ByReference t) ident) = (ident, t, ByRef)
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
  env <- ask
  if res
    then do
      (_, res) <- execStmt stmt
      return (env, res)
    else returnNothing
execStmt (CondElse cond a b) = do
  BoolVal res <- evalExpression cond
  env <- ask
  if res
    then do
      (_, res) <- execStmt a
      return (env, res)
    else do
      (_, res) <- execStmt b
      return (env, res)

-- while
execStmt (While e code) = do
  env <- ask
  BoolVal res <- evalExpression e
  if res
    then do
      execStmt code
      (_, res) <- execStmt (While e code)
      return (env, res)
    else return (env, Nothing)

-- for loop
execStmt (ForLoop init cond inc code) = do
  prvs_env <- ask
  execStmt init
  (env, _) <- execStmt init
  local (const env) $ do
    (_, res) <- execFor cond inc code
    return (prvs_env, res)

-- functions

execStmt (FnDef t ident args code) = do
  env <- ask
  res_env <- newMem ident (FuncVal (code, Data.List.map argToFuncArg args, t, env)) env
  return (res_env, Nothing)
execStmt (Ret expr) = do
  res <- evalExpression expr
  env <- ask
  return (env, Just res)
execStmt VRet = do
  env <- ask
  return (env, Just VoidVal)
execStmt (SExp e) = do
  evalExpression e
  returnNothing

-- tuples

execStmt (TupleUnpack (TupleUnpackStruct unpacked) expr) = do
  (TupleVal res) <- evalExpression expr
  env <- ask
  env_upd <- execUnpackTuple unpacked res env
  return (env_upd, Nothing)

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
