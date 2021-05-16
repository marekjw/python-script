module Eval where

import PythonScript.Abs
import Types

evalAddOp Minus e1 e2 = e1 - e2
evalAddOp Plus e1 e2 = e1 + e2

evalExpression :: Expr -> Context MemVal
-- math operations
evalExpression (EAdd e1 op e2) = do
  IntVal r1 <- evalExpression e1
  IntVal r2 <- evalExpression e2
  return $ IntVal $ evalAddOp op r1 r2

-- literals
evalExpression (EString s) = return $ StringVal s
evalExpression (ELitInt i) = return $ IntVal i
evalExpression ELitTrue = return $ BoolVal True
evalExpression ELitFalse = return $ BoolVal False

evalExpressions :: [Expr] -> Context [MemVal]
evalExpressions = mapM evalExpression