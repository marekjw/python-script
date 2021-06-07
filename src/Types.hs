module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as Map
import Data.Maybe
import PythonScript.Abs

type VariableName = String

type ReturnResult = Maybe MemVal

type Loc = Integer

type MyEnv = Map.Map VariableName Loc

data TupleVal
  = List TupleVal
  | MemVal

data PassType = ByValue | ByRef

type FunArg = (Ident, Type, PassType)

type FunArgList = [FunArg]

type FuncDef = (Block, FunArgList, Type)

data MemVal
  = BoolVal Bool
  | IntVal Integer
  | StringVal String
  | CharVal Char
  | FuncVal FuncDef
  | VoidVal

instance Show MemVal where
  show (BoolVal a) = show a
  show (IntVal a) = show a
  show (StringVal a) = show a
  show (CharVal a) = show a
  show VoidVal = "--void--"

instance Eq MemVal where
  (==) (BoolVal a) (BoolVal b) = a == b
  (==) (IntVal a) (IntVal b) = a == b
  (==) (StringVal a) (StringVal b) = a == b
  (==) (CharVal a) (CharVal b) = a == b

instance Ord MemVal where
  (<=) (BoolVal a) (BoolVal b) = a <= b
  (<=) (IntVal a) (IntVal b) = a <= b
  (<=) (StringVal a) (StringVal b) = a <= b
  (<=) (CharVal a) (CharVal b) = a <= b

type MyState = Map.Map Loc MemVal

type MyStore = (MyState, Loc)

data RuntimeExceptions
  = DivisionByZeroException
  | NoReturnException
  deriving (Show)

type Context = ReaderT MyEnv (StateT MyStore (ExceptT RuntimeExceptions IO))