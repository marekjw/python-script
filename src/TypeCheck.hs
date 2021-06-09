module Interpreter where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map as Map
import Data.Maybe
import PythonScript.Abs
import Tuples
import TypeEnv
import Types

-- TODO tuple
chckDeclareVar :: Type -> TypeEnv -> Item -> TypeContext TypeEnv
chckDeclareVar t env (Init name e) = do
  et <- checkExpression e
  if et /= t
    then throwError TypeError
    else newTypeMem name et env
chckDeclareVar t env (NoInit name) = do
  newTypeMem name t env

chckDeclareVars :: Type -> [Item] -> TypeContext TypeEnv
chckDeclareVars t items = do
  env <- ask
  foldM (chckDeclareVar t) env items

checkExpression :: Expr -> TypeContext Type
checkExpression _ = return Void

checkExpressions :: [Expr] -> TypeContext ()
checkExpressions exprs = do
  mapM_ checkExpression exprs

returnNothing :: TypeContext TypeEnv
returnNothing = do
  ask

checkStatement :: Stmt -> Type -> TypeContext TypeEnv
checkStatement (BStmt (Block stmts)) t = do
  env <- ask
  checkStatements stmts t
  return env
-- print
checkStatement (Print es) _ = do
  checkExpressions es
  returnNothing

-- variables
checkStatement (Decl t items) _ = do
  chckDeclareVars t items
checkStatement (Ass ident e) _ = do
  et <- checkExpression e
  it <- getTypeMem ident
  if et /= it
    then throwError TypeError
    else returnNothing
checkStatement (Incr ident) _ = do
  it <- getTypeMem ident
  case it of
    Int -> returnNothing
    _ -> throwError IncreasingNotInt
checkStatement (Decr ident) t = checkStatement (Incr ident) t
-- if
checkStatement (Cond cond stmt) t = do
  et <- checkExpression cond
  case et of
    Bool -> checkStatement stmt t
    _ -> throwError ConditionIsNotBool
checkStatement (CondElse cond a b) t = do
  et <- checkExpression cond
  case et of
    Bool -> do
      env <- checkStatement a t
      checkStatement b t
    _ -> throwError ConditionIsNotBool

-- while
checkStatement (While e code) t = do
  env <- ask
  et <- checkExpression e
  case et of
    Bool -> do
      checkStatement code t
    _ -> throwError ConditionIsNotBool

-- for loop
checkStatement (ForLoop init cond inc code) t = do
  env <- ask
  et <- checkExpression cond
  case et of
    Bool -> do
      checkStatement init t
      checkStatement inc t
      checkStatement code t
    _ -> throwError ConditionIsNotBool

-- functions

-- TODO def

checkStatement (Ret expr) t = do
  et <- checkExpression expr
  if et == t
    then returnNothing
    else throwError WrongReturnType
checkStatement VRet t =
  case t of
    Void -> returnNothing
    _ -> throwError WrongReturnType
-- tuples

-- generators

-- empty
checkStatement Empty _ = returnNothing

checkStatements :: [Stmt] -> Type -> TypeContext TypeEnv
checkStatements [] _ = returnNothing
checkStatements (stmt : rest) t = do
  env <- checkStatement stmt t
  local (const env) $ checkStatements rest t

checkProgram :: [Stmt] -> IO (Either TypeCheckExceptions TypeEnv)
checkProgram prog = runExceptT $ runReaderT (checkStatements prog Void) Map.empty