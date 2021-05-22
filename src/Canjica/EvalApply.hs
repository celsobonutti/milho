{-# LANGUAGE LambdaCase #-}
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
import Text.Megaparsec (parse, errorBundlePretty)
import Protolude hiding (MonadReader, ask, get, put, yield, local)
import Data.Text (pack)

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

apply (BuiltIn Add : as) =
  batchEval as <&> foldr add (Number 0)

apply (BuiltIn Mul : as) =
  batchEval as <&> foldr mul (Number 1)

apply [BuiltIn Def, Symbol name, atom] = do
  evaluatedAtom <- eval atom
  table <- get @"table"
  put @"table" (Map.insert name evaluatedAtom table)
  return (Symbol name)

apply (BuiltIn Defn : Symbol name : as) = do
  scope <- ask @"localScope"
  case Function.create scope as of
    Left e -> return $ Error e
    Right f -> do
      table <- get @"table"
      put @"table" (Map.insert name (Function f) table)
      return $ Symbol name

apply [BuiltIn If, predicate, consequent, alternative] =
  eval predicate
    >>= \case
      e@(Error _) -> return e
      Bool False -> eval alternative
      _ -> eval consequent

apply (BuiltIn Print : rest) =
  batchEval rest
    >>= putStr . unwords . map show
    >> return Nil

apply (BuiltIn Fn : rest) =
  ask @"localScope"
  >>= \scope -> return $ case Function.create scope rest of
    Left e -> Error e
    Right f -> Function f

apply (BuiltIn Eql : rest) = do
  evaluatedAtoms <- batchEval rest
  return $ case evaluatedAtoms of
    [] -> Error "Wrong number of arguments for =. Expecting at least one, found 0"
    (base : rest) ->
      Bool $ (== base) `all` rest

apply [BuiltIn Loop, procedure] =
  forever (eval procedure)

apply [BuiltIn Read] = do
  input <- liftIO getLine
  return $ case parse pAtomLine mempty input of
    Left e -> Error . pack . errorBundlePretty $ e
    Right a -> a

apply [BuiltIn Eval, value] =
  eval >=> eval $ value

apply [BuiltIn Quote, atom] =
  return atom

apply (Function fn : as) = do
  evaluatedArguments <- batchEval as
  let
    (newScope, vars) = Function.proceed fn evaluatedArguments
  local @"localScope" (\localScope -> Map.unions [newScope, scope fn, localScope]) (eval vars)

apply (List l : as) = do
  operator <- apply l
  apply (operator : as)

apply _ = return $ Error "Not implemented yet"
