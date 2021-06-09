module TypeEnv where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as Map
import Data.Maybe
import PythonScript.Abs
import Types

newTypeMem :: Ident -> Type -> TypeEnv -> TypeContext TypeEnv
newTypeMem (Ident i) et env = do
  let env_upd = Map.insert i et env
  return env_upd

getTypeMem :: Ident -> TypeContext Type
getTypeMem (Ident i) = do
  env <- ask
  let et = Map.lookup i env
  case et of
    Just et -> return et
    Nothing ->
      throwError $ VariableNotDeclared i