module Exec where

import Eval
import PythonScript.Abs
import Types

execStmt :: Stmt -> Context (MyEnv, ReturnResult)
execStmt (Print e) = do
  str <- evalExpression e
  liftIO $ putStr str
  returnNothing
