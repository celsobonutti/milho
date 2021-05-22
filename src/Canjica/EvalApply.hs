module Canjica.EvalApply where

import Canjica.State
import qualified Canjica.Function as Function
import Capability.State
import Capability.Reader
import Data.IORef
import qualified Data.Map as Map
import Pipoquinha.Types.Data
import Pipoquinha.Types.Atom
import Pipoquinha.Parser
import Protolude hiding (MonadReader, ask, get, put, yield, local)

eval :: (HasState "table" VarTable m, HasReader "localScope" VarTable m, MonadIO m) => Atom -> m Atom
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
      localScope <- ask @"localScope"
      table <- get @"table"
      return . fromMaybe (Error $ "Undefined variable: " <> name) . Map.lookup name . Map.union localScope $ table
    List l -> apply l

batchEval :: (HasState "table" VarTable m, HasReader "localScope" VarTable m, MonadIO m) => [Atom] -> m [Atom]
batchEval = traverse eval

apply :: (HasState "table" VarTable m, HasReader "localScope" VarTable m, MonadIO m) => [Atom] -> m Atom

apply [] = return Nil

apply (s@(Symbol _) : as) = do
  operator <- eval s
  apply (operator : as)

apply (BuiltIn Add : as) = do
  evaluatedArguments <- batchEval as
  return (foldr add (Number 0) evaluatedArguments)

apply (BuiltIn Mul : as) = do
  evaluatedArguments <- batchEval as
  return (foldr mul (Number 1) evaluatedArguments)

apply [BuiltIn Def, Symbol name, atom] = do
  evaluatedAtom <- eval atom
  table <- get @"table"
  put @"table" (Map.insert name evaluatedAtom table)
  return (Symbol name)

apply (BuiltIn Defn : Symbol name : as) =
  case Function.create as of
    Left e -> return $ Error e
    Right f -> do
      table <- get @"table"
      put @"table" (Map.insert name (Function f) table)
      return $ Symbol name

apply [BuiltIn If, predicate, consequent, alternative] = do
  pred <- eval predicate
  print pred
  case pred of
    e@(Error _) -> return e
    Bool False -> eval alternative
    _ -> eval consequent

apply (BuiltIn Print : rest) = do
  evaluatedAtoms <- batchEval rest
  putStrLn . unwords . map show $ evaluatedAtoms
  return Nil

apply (BuiltIn Fn : rest) = do
  case Function.create rest of
    Left e -> return $ Error e
    Right f -> return $ Function f

apply (Function fn : as) = do
  evaluatedArguments <- batchEval as
  let
    (newScope, vars) = Function.proceed fn evaluatedArguments
  local @"localScope" (Map.union newScope) (eval vars)

apply (List l : as) = do
  operator <- apply l
  apply (operator : as)

apply _ = return $ Error "Not implemented yet"
