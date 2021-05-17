module Variables where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as Map
import Eval
import PythonScript.Abs
import Types

initVariable :: Ident -> MemVal -> MyEnv -> Context MyEnv
initVariable (Ident i) val env = do
  (store, loc) <- get
  let env_updated = Map.insert i loc env
  put (Map.insert loc val store, loc + 1)
  return env_updated

declareVariable :: Type -> MyEnv -> Item -> Context MyEnv
declareVariable t env (Init name e) = do
  val <- evalExpression e
  initVariable name val env

-- declareVariable t env (NoInit name) =
-- initVariable name  env

declareVariables :: Type -> [Item] -> Context MyEnv
declareVariables t items = do
  env <- ask
  foldM (declareVariable t) env items
