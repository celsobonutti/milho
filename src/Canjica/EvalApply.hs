{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module Canjica.EvalApply where

import qualified Canjica.Function              as Function
import           Capability.Reader       hiding ( (:.:) )
import           Capability.State        hiding ( (:.:) )
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Text                      ( pack )
import           Pipoquinha.Parser
import           Protolude               hiding ( MonadReader
                                                , ask
                                                , get
                                                , local
                                                , put
                                                , yield
                                                )
import           Text.Megaparsec                ( errorBundlePretty
                                                , parse
                                                )

eval :: (HasReader "environment" Environment m, MonadIO m) => Atom -> m Atom
eval atom = case atom of
  n@(Number   _) -> return n
  f@(Function _) -> return f
  m@(Macro    _) -> return m
  b@(Bool     _) -> return b
  e@(Error    _) -> return e
  s@(Str      _) -> return s
  b@(BuiltIn  _) -> return b
  Symbol name    -> do
    environment <- ask @"environment"
    liftIO
      . fmap (fromMaybe (Error $ "Undefined variable: " <> name))
      . getValue name
      $ environment
  Pair (List l ) -> apply l
  Pair (_ :.: _) -> return $ Error "Cannot apply dotted pair"
  Pair _         -> return $ Pair Nil

batchEval
  :: (HasReader "environment" Environment m, MonadIO m) => [Atom] -> m [Atom]
batchEval = traverse eval

apply :: (HasReader "environment" Environment m, MonadIO m) => [Atom] -> m Atom

apply []                     = return $ Pair Nil

apply (BuiltIn Add : as)     = batchEval as <&> foldr add (Number 0)

apply [BuiltIn Negate, atom] = eval atom >>= \case
  Number x -> return . Number . negate $ x
  e@(Error _) -> return e
  _ -> return . Error $ "Invalid parameter for 'negate': expecting a number"

apply (BuiltIn Mul : as)     = batchEval as <&> foldr mul (Number 1)

apply [BuiltIn Invert, atom] = eval atom >>= \case
  Number x -> return . Number $ denominator x % numerator x
  e@(Error _) -> return e
  _ -> return . Error $ "Invalid parameter for 'invert': expecting a number"

apply [BuiltIn Numerator, atom] = eval atom >>= \case
  Number x -> return . Number $ numerator x % 1
  e@(Error _) -> return e
  _ -> return . Error $ "Invalid parameter for 'numerator': expecting a number"

apply [BuiltIn Def, Symbol name, atom] = do
  evaluatedAtom <- eval atom
  environment   <- ask @"environment"
  liftIO (setValue name evaluatedAtom environment)
  return (Symbol name)

apply (BuiltIn Defn : Symbol name : rest) = do
  environment <- ask @"environment"
  case Function.create environment rest of
    Left  e -> return $ Error e
    Right f -> do
      environment <- ask @"environment"
      liftIO (setValue name (Function f) environment)
      return $ Symbol name

apply [BuiltIn If, predicate, consequent, alternative] =
  eval predicate >>= \case
    e@(Error _) -> return e
    Bool False  -> eval alternative
    _           -> eval consequent

apply [BuiltIn Not, value] = eval value >>= \case
  e@(Error _) -> return e
  Bool False  -> return . Bool $ True
  _           -> return . Bool $ False

apply (BuiltIn Print : rest) =
  batchEval rest >>= putStr . unwords . map show >> return (Pair Nil)

apply (BuiltIn Fn : rest) = ask @"environment" >>= \environment ->
  return $ case Function.create environment rest of
    Left  e -> Error e
    Right f -> Function f

apply (BuiltIn Eql : rest) = batchEval rest >>= \case
  [] -> return
    (Error "Wrong number of arguments for =. Expecting at least one, found 0")
  (base : rest) -> return (Bool $ (== base) `all` rest)

apply [BuiltIn Loop, procedure] = forever (eval procedure)

apply [BuiltIn Read]            = do
  input <- liftIO getLine
  return $ case parse pAtomLine mempty input of
    Left  e -> Error . pack . errorBundlePretty $ e
    Right a -> a

apply [BuiltIn Eval , value]   = eval >=> eval $ value

apply [BuiltIn Quote, atom ]   = return atom

apply (BuiltIn Do : rest)      = foldM (const eval) (Pair Nil) rest

apply [BuiltIn Cons, car, cdr] = do
  car <- eval car
  cdr <- eval cdr
  return $ Pair (car :.: cdr)

apply [BuiltIn Car, atom] = eval atom >>= \case
  Pair Nil       -> return $ Pair Nil
  Pair (x ::: _) -> return x
  Pair (x :.: _) -> return x
  _              -> return $ Error "Car can only be applied to pairs"

apply [BuiltIn Cdr, atom] = eval atom >>= \case
  Pair Nil        -> return $ Pair Nil
  Pair (_ ::: xs) -> return $ Pair xs
  Pair (_ :.: x ) -> return x
  _               -> return $ Error "Cdr can only be applied to pairs"

apply [BuiltIn Set, Symbol name, atom] = do
  evaluatedAtom <- eval atom
  environment   <- ask @"environment"
  varEnv        <- liftIO (getVariableEnvironment name environment)
  case varEnv of
    Nothing -> do
      return . Error $ "Cannot set! on undefined variable: " <> name
    Just varEnv -> do
      liftIO (setValue name evaluatedAtom varEnv)
      return (Symbol name)

apply (BuiltIn _    : _ ) = return $ Error "Not implemented yet"

apply (s@(Symbol _) : as) = do
  operator <- eval s
  apply (operator : as)

apply (Function fn : as) = do
  evaluatedArguments <- batchEval as
  case Function.proceed evaluatedArguments fn of
    Left  e                             -> return $ Error e
    Right (newScope, environment, vars) -> do
      table <- liftIO . newIORef $ newScope
      let newEnvironment = Local { table, parent = environment }
      local @"environment" (const newEnvironment) (eval vars)

apply (Macro _        : _ ) = return $ Error "Macros are not implemented yet"

apply (Pair  (List l) : as) = do
  operator <- apply l
  apply (operator : as)

apply (Pair (_ :.: _) : _) =
  return $ Error "Cannot apply dotted pair as an operator"

apply (Pair   Nil  : _) = return $ Error "Cannot apply Nil as an operator"

apply (Bool   _    : _) = return $ Error "Cannot apply boolean as an operator"

apply (Str    _    : _) = return $ Error "Cannot apply string as an operator"

apply (Number _    : _) = return $ Error "Cannot apply number as an operator"

apply (e@(Error _) : _) = return e
