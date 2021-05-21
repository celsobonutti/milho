module Canjica.EvalApply where

import Canjica.State
import Capability.Reader
import Capability.State
import Data.IORef
import qualified Data.Map as Map
import Pipoquinha.Types.Data
import Pipoquinha.Parser
import Protolude hiding (MonadReader, ask, get, put)

eval :: (HasState "table" VarTable m, MonadIO m) => Atom -> m Atom
eval atom =
  case atom of
    n@(Number _) -> return n
    f@(Function _) -> return f
    m@(Macro _) -> return m
    m@(MultiArityFn _) -> return m
    b@(Bool _) -> return b
    e@(Error _) -> return e
    s@(Str _) -> return s
    b@(BuiltIn _) -> return b
    Nil -> return Nil
    Symbol name -> do
      table <- get @"table"
      return . fromMaybe (Error $ "Undefined variable: " <> name) . Map.lookup name $ table
    List l -> apply l

apply :: (HasState "table" VarTable m, MonadIO m) => [Atom] -> m Atom
apply [] = return Nil

apply [BuiltIn Def, Symbol name, atom] = do
  evaluatedAtom <- eval atom
  table <- get @"table"
  put @"table" (Map.insert name evaluatedAtom table)
  return (Symbol name)

apply _ = return $ Error "Not implemented yet"

