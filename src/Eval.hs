module Eval where

import PythonScript.Abs
import Types

evalAddOp Minus x y = x - y
evalAddOp Plus x y = x + y

evalRelOp GTH x y = x > y
evalRelOp GE x y = x >= y
evalRelOp LTH x y = x < y
evalRelOp EQU x y = x == y
evalRelOp NE x y = x /= y
evalRelOp LE x y = x <= y

evalExpression :: Expr -> Context MemVal
evalExpression (EAdd x op y) = do
  IntVal r1 <- evalExpression x
  IntVal r2 <- evalExpression y
  return $ IntVal $ evalAddOp op r1 r2
evalExpression (ERel a op b) = do
  r1 <- evalExpression a
  r2 <- evalExpression b
  return $ BoolVal $ evalRelOp op r1 r2
evalExpression (EString s) = return $ StringVal s
evalExpression (ELitInt i) = return $ IntVal i
evalExpression ELitTrue = return $ BoolVal True
evalExpression ELitFalse = return $ BoolVal False
evalExpression (ELitChar c) = return $ CharVal c

evalExpressions :: [Expr] -> Context [MemVal]
evalExpressions = mapM evalExpression