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

data MemVal = BoolVal Bool | IntVal Integer | StringVal String | VoidVal | CharVal Char

instance Show MemVal where
  show (BoolVal a) = show a
  show (IntVal a) = show a
  show (StringVal a) = show a
  show (CharVal a) = show a

type MyState = Map.Map Loc MemVal

type MyStore = (MyState, Loc)

data RuntimeExceptions = DivisionByZeroException | NoReturnException | NoStructFieldException String | OutOfRangeExeption Integer deriving (Show)

type Context = ReaderT MyEnv (StateT MyStore (ExceptT RuntimeExceptions IO))