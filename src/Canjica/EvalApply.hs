{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Canjica.EvalApply where

import qualified Canjica.Function as Function
import Canjica.State
import Capability.Reader
import Capability.State
import Data.IORef
import qualified Data.Map as Map
import Data.Text (pack)
import Pipoquinha.Parser
import Pipoquinha.Types.Atom
import Pipoquinha.Types.Data
import Protolude hiding (MonadReader, ask, get, local, put, yield)
import Text.Megaparsec (errorBundlePretty, parse)

eval :: (HasState "table" VarTable m, HasReader "localScope" VarTable m, MonadIO m) => Atom -> m Atom
eval atom =
  case atom of
    n@(Number _) -> return n
    f@(Function _) -> return f
    m@(Macro _) -> return m
    b@(Bool _) -> return b
    e@(Error _) -> return e
    s@(Str _) -> return s
    b@(BuiltIn _) -> return b
    Symbol name -> do
      localScope <- ask @"localScope"
      table <- get @"table"
      return . fromMaybe (Error $ "Undefined variable: " <> name) . Map.lookup name . Map.union localScope $ table
    List l -> apply l

batchEval :: (HasState "table" VarTable m, HasReader "localScope" VarTable m, MonadIO m) => [Atom] -> m [Atom]
batchEval = traverse eval

apply :: (HasState "table" VarTable m, HasReader "localScope" VarTable m, MonadIO m) => [Atom] -> m Atom
apply [] = return (List [])
apply (BuiltIn Add : as) =
  batchEval as <&> foldr add (Number 0)
apply (BuiltIn Mul : as) =
  batchEval as <&> foldr mul (Number 1)
apply [BuiltIn Def, Symbol name, atom] = do
  evaluatedAtom <- eval atom
  table <- get @"table"
  put @"table" (Map.insert name evaluatedAtom table)
  return (Symbol name)
apply (BuiltIn Defn : Symbol name : rest) = do
  scope <- ask @"localScope"
  case Function.create scope rest of
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
    >> return (List [])
apply (BuiltIn Fn : rest) =
  ask @"localScope"
    >>= \scope -> return $ case Function.create scope rest of
      Left e -> Error e
      Right f -> Function f
apply (BuiltIn Eql : rest) =
  batchEval rest
    >>= \case
      [] -> return (Error "Wrong number of arguments for =. Expecting at least one, found 0")
      (base : rest) ->
        return (Bool $ (== base) `all` rest)
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
apply (BuiltIn Do : rest) =
  foldM (const eval) (List []) rest
apply [BuiltIn Cons, car, cdr] =
  eval cdr
    >>= \case
      List l ->
        eval car <&> List . (: l)
      _ -> return (Error "Can only cons into lists")
apply [BuiltIn Car, atom] =
  eval atom
    >>= \case
      List [] -> return $ List []
      List (x : _) -> return x
      _ -> return $ Error "Car can only be applied to lists"
apply [BuiltIn Cdr, atom] =
  eval atom
    >>= \case
      List [] -> return $ List []
      List (_ : xs) -> return $ List xs
      _ -> return $ Error "Cdr can only be applied to lists"
apply (BuiltIn _ : _) =
  return $ Error "Not implemented yet"
apply (s@(Symbol _) : as) = do
  operator <- eval s
  apply (operator : as)
apply (Function fn : as) = do
  evaluatedArguments <- batchEval as
  let (newScope, vars) = Function.proceed fn evaluatedArguments
  local @"localScope" (\localScope -> Map.unions [newScope, scope fn, localScope]) (eval vars)
apply (Macro _ : _) =
  return $ Error "Macros are not implemented yet"
apply (List l : as) = do
  operator <- apply l
  apply (operator : as)
apply (Bool _ : _) =
  return $ Error "Cannot apply boolean as an operator"
apply (Str _ : _) =
  return $ Error "Cannot apply string as an operator"
apply (Number _ : _) =
  return $ Error "Cannot apply number as an operator"
apply (e@(Error _) : _) =
  return e
