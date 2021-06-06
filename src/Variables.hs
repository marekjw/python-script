module Variables where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Env
import Eval
import PythonScript.Abs
import Types

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
