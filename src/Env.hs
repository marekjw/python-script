module Env where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as Map
import Data.Maybe
import PythonScript.Abs
import Types

newMem :: Ident -> MemVal -> MyEnv -> Context MyEnv
newMem (Ident i) val env = do
  (store, loc) <- get
  let env_updated = Map.insert i loc env
  put (Map.insert loc val store, loc + 1)
  return env_updated

findLoc :: Ident -> Context (Maybe Loc)
findLoc (Ident i) = do
  asks (Map.lookup i)

rewireMem :: Ident -> MyEnv -> Context MyEnv
rewireMem (Ident i) env = do
  loc <- findLoc (Ident i)
  case loc of
    Nothing -> throwError VariableNotFound
    Just l -> do
      let env_updated = Map.insert i l env
      return env_updated

setMem :: Ident -> MemVal -> Context ()
setMem i val = do
  (store, next_loc) <- get
  locRes <- findLoc i
  case locRes of
    Just loc ->
      put (Map.insert loc val store, next_loc)
    Nothing -> do
      liftIO $ putStrLn "Variable not found"

getMem :: Ident -> Context MemVal
getMem (Ident i) = do
  env <- ask
  let locRes = Map.lookup i env
  case locRes of
    Just loc -> do
      (store, _) <- get
      let Just val = Map.lookup loc store
      return val
    Nothing -> do
      liftIO $ putStrLn "Variable not found"
      return (IntVal 0)