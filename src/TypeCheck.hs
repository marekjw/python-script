module TypeCheck where

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

parseFuncArg :: Arg -> FunType
parseFuncArg (Arg (ByReference (LambdaFun args t)) _) =
  ByReference $ Fun t args
parseFuncArg (Arg (PythonScript.Abs.ByValue (LambdaFun args t)) _) =
  PythonScript.Abs.ByValue $ Fun t args
parseFuncArg (Arg t _) = t

checkFunArgs :: [FunType] -> [Expr] -> TypeContext ()
checkFunArgs [] [] = pure ()
checkFunArgs (farg : frest) (expr : erest) = do
  case farg of
    PythonScript.Abs.ByValue t -> do
      et <- checkExpression expr
      if t /= et
        then throwError TypeError
        else checkFunArgs frest erest
    ByReference t -> do
      case expr of
        EVar ident -> do
          et <- getTypeMem ident
          if et /= t
            then throwError TypeError
            else checkFunArgs frest erest
        _ -> throwError CannotPassValueBuReference
checkFunArgs _ _ = throwError WrongArgumentCount

checkTupleUnpack :: [Type] -> [UnpackedArg] -> TypeEnv -> TypeContext TypeEnv
checkTupleUnpack [] [] env = return env
checkTupleUnpack (t : trest) (arg : arest) env = do
  case (t, arg) of
    (atype, UnpackAssign ident) -> do
      at <- getTypeMem ident
      if atype /= at
        then throwError TypeError
        else checkTupleUnpack trest arest env
    (atype, UnpackDeclare t ident) -> do
      if atype /= t
        then throwError TypeError
        else do
          env_upd <- newTypeMem ident t env
          checkTupleUnpack trest arest env_upd
    (TupleType types, UnpackRec (TupleUnpackStruct args)) -> do
      env_upd <- checkTupleUnpack types args env
      checkTupleUnpack trest arest env_upd
    (_, _) -> throwError TypeError
checkTupleUnpack _ _ _ = throwError TypeError

initTypeArgs :: [Arg] -> TypeEnv -> TypeContext TypeEnv
initTypeArgs [] res = return res
initTypeArgs (Arg ftype ident : rest) env = do
  case ftype of
    ByReference (LambdaFun args ltype) ->
      initTypeArgs (Arg (ByReference (Fun ltype args)) ident : rest) env
    PythonScript.Abs.ByValue (LambdaFun args ltype) ->
      initTypeArgs (Arg (PythonScript.Abs.ByValue (Fun ltype args)) ident : rest) env
    ByReference t -> do
      next_env <- newTypeMem ident t env
      initTypeArgs rest next_env
    PythonScript.Abs.ByValue t -> do
      next_env <- newTypeMem ident t env
      initTypeArgs rest next_env

chckDeclareVar :: Type -> TypeEnv -> Item -> TypeContext TypeEnv
chckDeclareVar (LambdaFun args t) env item =
  chckDeclareVar (Fun t args) env item
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
-- literals
checkExpression (EString _) = return Str
checkExpression (ELitInt _) = return Int
checkExpression ELitTrue = return Bool
checkExpression ELitFalse = return Bool
checkExpression (ELitChar _) = return Char
-- variables
checkExpression (EVar ident) =
  getTypeMem ident
-- math
checkExpression (EAdd x op y) = do
  xt <- checkExpression x
  yt <- checkExpression y
  case (xt, yt) of
    (Int, Int) -> return Int
    _ -> throwError CannotDoMathOnNotInt
checkExpression (Neg e) = do
  et <- checkExpression e
  case et of
    Int -> return Int
    _ -> throwError CannotDoMathOnNotInt
checkExpression (EMul x _ y) = do
  xt <- checkExpression x
  yt <- checkExpression y
  case (xt, yt) of
    (Int, Int) -> return Int
    _ -> throwError CannotDoMathOnNotInt

-- logical operations
checkExpression (ERel a op b) = do
  at <- checkExpression a
  bt <- checkExpression b
  case (at, op, bt) of
    (Int, _, Int) -> return Bool
    (Char, _, Char) -> return Bool
    (Bool, _, Bool) -> return Bool
    (Str, _, Str) -> return Bool
    (TupleType _, EQU, TupleType _) -> return Bool
    (_, _, _) -> throwError TypeError

-- call function
checkExpression (EApp ident args) = do
  ftype <- getTypeMem ident
  case ftype of
    Fun t fargs -> do
      checkFunArgs fargs args
      return t
    _ -> do
      let (Ident name) = ident
      throwError $ IsNotCallable name

-- lambda function

checkExpression (LambdaFunVal args t code) = do
  env <- ask
  f_env <- initTypeArgs args env
  local (const f_env) (checkStatement code t)
  return $ Fun t (Data.List.map parseFuncArg args)

-- tuples expressions
checkExpression (ETuple exprs) = do
  res <- checkExpressions exprs
  return $ TupleType res
-- TODO delete this
checkExpression _ = return Void

checkExpressions :: [Expr] -> TypeContext [Type]
checkExpressions exprs = do
  mapM checkExpression exprs

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
  for_env <- checkStatement init t
  local
    (const for_env)
    ( do
        et <- checkExpression cond
        case et of
          Bool -> do
            checkStatement inc t
            checkStatement code t
            return env
          _ -> throwError ConditionIsNotBool
    )

-- functions
checkStatement (FnDef t ident args code) _ = do
  env <- ask
  res_env <- newTypeMem ident (Fun t (Data.List.map parseFuncArg args)) env
  f_env <- initTypeArgs args res_env
  let (Block stmts) = code
  local (const f_env) $ checkStatements stmts t
  return res_env
checkStatement (Ret expr) t = do
  et <- checkExpression expr
  if et == t
    then returnNothing
    else throwError WrongReturnType
checkStatement VRet t =
  case t of
    Void -> returnNothing
    _ -> throwError WrongReturnType
-- expression statement
checkStatement (SExp e) _ = do
  checkExpression e
  returnNothing

-- tuples
checkStatement (TupleUnpack (TupleUnpackStruct unpacked) expr) _ = do
  et <- checkExpression expr
  case et of
    TupleType types -> do
      env <- ask
      checkTupleUnpack types unpacked env
    _ -> throwError TypeError

-- generators

-- empty
checkStatement Empty _ = returnNothing
checkStatement _ _ = returnNothing

checkStatements :: [Stmt] -> Type -> TypeContext TypeEnv
checkStatements [] _ = returnNothing
checkStatements (stmt : rest) t = do
  env <- checkStatement stmt t
  local (const env) $ checkStatements rest t

checkProgram :: [Stmt] -> IO (Either TypeCheckExceptions TypeEnv)
checkProgram prog = runExceptT $ runReaderT (checkStatements prog Void) Map.empty