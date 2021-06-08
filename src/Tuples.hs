module Tuples where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.List
import Data.Map as Map
import Data.Maybe
import Env
import PythonScript.Abs
import Types

execUnpackTuple :: [UnpackedArg] -> [MemVal] -> MyEnv -> Context MyEnv
execUnpackTuple [] [] env = return env
execUnpackTuple (unpack : restUnpack) (val : restVal) env =
  case unpack of
    UnpackDeclare t ident -> do
      env_upd <- newMem ident val env
      execUnpackTuple restUnpack restVal env_upd
    UnpackAssign ident -> do
      setMem ident val
      execUnpackTuple restUnpack restVal env
    UnpackRec (TupleUnpackStruct unpackArgs) -> do
      case val of
        TupleVal memVals -> do
          env_upd <- execUnpackTuple unpackArgs memVals env
          execUnpackTuple restUnpack restVal env_upd
        _ -> throwError TupleMatchError
execUnpackTuple _ _ _ = throwError TupleMatchError
