module Env where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map as Map
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