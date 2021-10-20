module Canjica.EvalApply where

import qualified Canjica.Boolean               as Boolean
import           Canjica.Environment            ( basicEnvironment )
import qualified Canjica.Function              as Function
import           Canjica.Function               ( ProceedResult(..)
                                                , functionArguments
                                                , functionBody
                                                , functionEnvironment
                                                )
import qualified Canjica.Import                as Import
import           Canjica.Import                 ( ImportInformation(..) )
import qualified Canjica.Let                   as Let
import qualified Canjica.List                  as List
import qualified Canjica.Macro                 as Macro
import qualified Canjica.Number                as Number
import qualified Canjica.String                as String
import           Capability.Error        hiding ( (:.:)
                                                , catchJust
                                                )
import           Capability.Reader       hiding ( (:.:) )
import           Capability.State        hiding ( (:.:) )
import           Control.Exception.Base         ( runtimeError )
import           Data.IORef
import qualified Data.Map                      as Map
import           Pipoquinha.BuiltIn      hiding ( T )
import qualified Pipoquinha.Environment        as Environment
import           Pipoquinha.Environment         ( CatchCapable
                                                , ReaderCapable
                                                , StateCapable
                                                )
import           Pipoquinha.Error               ( ExpectedType(..)
                                                , T(..)
                                                )
import qualified Pipoquinha.Error              as Error
import           Pipoquinha.Parser             as Parser
import           Pipoquinha.SExp         hiding ( T )
import qualified Pipoquinha.SExp               as SExp
import qualified Pipoquinha.Type               as Type
import qualified Protolude
import           Protolude               hiding ( MonadReader
                                                , ask
                                                , asks
                                                , catch
                                                , get
                                                , gets
                                                , local
                                                , put
                                                , yield
                                                )
import           System.Directory               ( makeAbsolute )
import           System.FilePath                ( takeDirectory )
import           Text.Megaparsec                ( errorBundlePretty
                                                , parse
                                                )

throwIfError :: CatchCapable m => SExp.Result a -> m a
throwIfError = \case
    Left  error -> throw @"runtimeError" error
    Right value -> return value

eval
    :: (ReaderCapable SExp.T m, CatchCapable m, StateCapable SExp.T m)
    => SExp.T
    -> m SExp.T
eval atom = case atom of
    n@(Number   _) -> return n
    f@(Function _) -> return f
    m@(Macro    _) -> return m
    b@(Bool     _) -> return b
    e@(Error    _) -> return e
    s@(String   _) -> return s
    b@(BuiltIn  _) -> return b
    Symbol name    -> do
        join (asks @"table" (Environment.getValue name)) >>= liftIO . readIORef
    Pair (List l ) -> apply l
    Pair (_ :.: _) -> throw @"runtimeError" $ CannotApply Type.Pair
    Pair _         -> return $ Pair Nil

batchEval
    :: (ReaderCapable SExp.T m, CatchCapable m, StateCapable SExp.T m)
    => [SExp.T]
    -> m [SExp.T]
batchEval = traverse eval

apply
    :: (ReaderCapable SExp.T m, CatchCapable m, StateCapable SExp.T m)
    => [SExp.T]
    -> m SExp.T

apply []                        = return $ Pair Nil

{- Comparison operations -}

apply (BuiltIn Eql : arguments) = batchEval arguments >>= \case
    [] -> throw @"runtimeError" $ NotEnoughArguments { expectedCount = 1
                                                     , foundCount    = 0
                                                     , functionName  = Just "="
                                                     }

    (base : rest) -> return (Bool $ (== base) `all` rest)

apply (BuiltIn Gt : arguments) = batchEval arguments >>= \case
    [] -> throw @"runtimeError" $ NotEnoughArguments { expectedCount = 1
                                                     , foundCount    = 0
                                                     , functionName  = Just ">"
                                                     }
    arguments -> do
        isGreater <- throwIfError
            (mapM (uncurry Number.gt) (List.pairs arguments))
        throwIfError (foldM Boolean.and (Bool True) isGreater)

apply (BuiltIn Lt : arguments) = batchEval arguments >>= \case
    [] -> throw @"runtimeError" $ NotEnoughArguments { expectedCount = 1
                                                     , foundCount    = 0
                                                     , functionName  = Just "<"
                                                     }
    arguments -> do
        isGreater <- throwIfError
            (mapM (uncurry Number.lt) (List.pairs arguments))
        throwIfError (foldM Boolean.and (Bool True) isGreater)

{- Number operations -}

apply (BuiltIn Add : arguments) =
    batchEval arguments <&> foldM Number.add (Number 0) >>= throwIfError

apply (BuiltIn Mul : arguments) =
    batchEval arguments <&> foldM Number.mul (Number 1) >>= throwIfError

apply [BuiltIn Negate, atom] = eval atom >>= \case
    Number x -> return . Number . negate $ x
    value -> throw @"runtimeError" TypeMismatch { expected = Simple Type.Number
                                                , found    = SExp.toType value
                                                }
apply (BuiltIn Negate : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "negate"
        }

apply [BuiltIn Invert, atom] = eval atom >>= \case
    Number 0 -> throw @"runtimeError" DividedByZero
    Number x -> return . Number $ denominator x % numerator x
    value -> throw @"runtimeError" TypeMismatch { expected = Simple Type.Number
                                                , found    = SExp.toType value
                                                }

apply (BuiltIn Invert : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "invert"
        }

apply [BuiltIn Numerator, atom] = eval atom >>= \case
    Number x -> return . Number $ numerator x % 1
    value -> throw @"runtimeError" TypeMismatch { expected = Simple Type.Number
                                                , found    = SExp.toType value
                                                }

apply (BuiltIn Numerator : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "numerator"
        }

{- Control flow operations -}

apply [BuiltIn If, predicate, consequent, alternative] =
    eval predicate >>= \case
        Bool False -> eval alternative
        _          -> eval consequent

apply (BuiltIn If : arguments) = throw @"runtimeError" $ WrongNumberOfArguments
    { expectedCount = 3
    , foundCount    = length arguments
    , functionName  = Just "if"
    }

apply (BuiltIn Cond : arguments) = case arguments of
    [] -> throw @"runtimeError" $ NotEnoughArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "cond"
        }

    arguments | Just conditions <- mapM validateCondition arguments -> do
        evaluated <- mapM evaluateCondition conditions
        case find ((/= Bool False) . fst) evaluated of
            Nothing                      -> return $ Pair Nil
            Just (condition, consequent) -> eval consequent

    _ -> throw @"runtimeError" $ MalformedCond

  where
    validateCondition (Pair (List [condition, consequent])) =
        Just (condition, consequent)
    validateCondition _ = Nothing

    evaluateCondition (condition, consequent) = do
        evaluatedCondition <- eval condition
        return (evaluatedCondition, consequent)

{-  Variable definition/mutation operations
    def, defn, defmacro, set!, let, guard   -}

apply [BuiltIn Def, Symbol name, atom] = do
    evaluatedSExp <- eval atom
    environment   <- ask @"table"
    Environment.insertValue name evaluatedSExp
    return (Symbol name)

apply (BuiltIn Def : arguments) = throw @"runtimeError" $ MalformedDefinition

apply (BuiltIn Defn : Symbol name : rest) = do
    environment <- ask @"table"
    function    <- throwIfError $ Function.make environment (Just name) rest
    Environment.insertValue name (Function function)
    return $ Symbol name

apply (BuiltIn Defn : arguments) = throw @"runtimeError" $ MalformedDefinition

apply (BuiltIn Defmacro : Symbol name : rest) = do
    environment <- ask @"table"
    function    <- throwIfError $ Function.make environment (Just name) rest
    Environment.insertValue name (Macro function)
    return $ Symbol name

apply (BuiltIn Defmacro : arguments) =
    throw @"runtimeError" $ MalformedDefinition

apply [BuiltIn Set, Symbol name, atom] = do
    evaluatedSExp <- eval atom
    environment   <- ask @"table"
    Environment.setValue name evaluatedSExp environment
    return (Symbol name)

apply (BuiltIn Set : _)                     = throw @"runtimeError" MalformedSet

apply [BuiltIn Let, Pair (List exps), body] = do
    letTable           <- throwIfError $ Let.makeTable exps

    environment        <- ask @"table"

    evaluatedArguments <- mapM eval letTable

    newScope           <- liftIO . mapM newIORef $ evaluatedArguments

    environmentRef     <- liftIO . newIORef $ Environment.Table
        { variables = newScope
        , parent    = Just environment
        }

    local @"table" (const environmentRef) (eval body)

apply (BuiltIn Let : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 2
        , foundCount    = length arguments
        , functionName  = Just "let"
        }

{-  Function/macro operations
    fn, funcion application and macro expansion -}

apply (BuiltIn Fn : rest) = do
    environment <- ask @"table"
    function    <- throwIfError $ Function.make environment Nothing rest
    return $ Function function

apply (Function fn : values) = do
    evaluated      <- batchEval values
    result         <- throwIfError $ Function.proceed fn evaluated
    newScope       <- liftIO . mapM newIORef $ functionArguments result

    environmentRef <- liftIO . newIORef $ Environment.Table
        { variables = newScope
        , parent    = Just $ functionEnvironment result
        }

    local @"table" (const environmentRef) (eval $ functionBody result)

apply (Macro macro : values) = do
    ProceedResult { functionArguments, functionEnvironment, functionBody } <-
        throwIfError $ Function.proceed macro values

    let expanded = Macro.expand functionArguments functionBody

    local @"table" (const functionEnvironment) (eval expanded)

{-  REPL
    Read, Eval, Print, Loop -}

apply [BuiltIn Read] = do
    input <- liftIO getLine
    return $ case parse Parser.sExpLine mempty input of
        Left  e -> Error . ParserError . toS . errorBundlePretty $ e
        Right a -> a

apply (BuiltIn Read : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 0
        , foundCount    = length arguments
        , functionName  = Just "read"
        }

apply [BuiltIn Eval, value] = eval >=> eval $ value

apply (BuiltIn Eval : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "eval"
        }

apply (BuiltIn PrintLn : rest) =
    batchEval rest >>= putStrLn . unwords . map showUnlessString >> return
        (Pair Nil)
  where
    showUnlessString (String s) = s
    showUnlessString other      = show other

apply (BuiltIn Print : rest) =
    batchEval rest >>= putStr . unwords . map showUnlessString >> return
        (Pair Nil)
  where
    showUnlessString (String s) = s
    showUnlessString other      = show other

apply [BuiltIn Loop, procedure] = forever (eval procedure)

apply (BuiltIn Loop : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "loop"
        }

apply [BuiltIn Quote, atom] = return atom

apply (BuiltIn Quote : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "quote"
        }


apply (BuiltIn Do : rest)      = foldM (const eval) (Pair Nil) rest

{-  List operations
    cons, car, cdr -}

apply [BuiltIn Cons, car, cdr] = do
    car <- eval car
    cdr <- eval cdr
    return $ Pair (car :.: cdr)

apply (BuiltIn Cons : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 2
        , foundCount    = length arguments
        , functionName  = Just "cons"
        }

apply [BuiltIn Car, atom] = eval atom >>= \case
    Pair Nil       -> return $ Pair Nil
    Pair (x ::: _) -> return x
    Pair (x :.: _) -> return x
    value -> throw @"runtimeError" TypeMismatch { expected = Simple Type.Pair
                                                , found    = SExp.toType value
                                                }

apply (BuiltIn Car : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "car"
        }

apply [BuiltIn Cdr, atom] = eval atom >>= \case
    Pair Nil        -> return $ Pair Nil
    Pair (_ ::: xs) -> return $ Pair xs
    Pair (_ :.: x ) -> return x
    value -> throw @"runtimeError" TypeMismatch { expected = Simple Type.Pair
                                                , found    = SExp.toType value
                                                }

apply (BuiltIn Cdr : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "cdr"
        }

{-  String operations
    str, concat, split -}

apply [BuiltIn Str, argument] = eval argument >>= \case
    String s -> return $ String s
    value    -> return . String . show $ value

apply (BuiltIn Str : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "str"
        }

apply [BuiltIn Concat, first, second] = do
    fst <- eval first
    snd <- eval second
    throwIfError $ String.concat fst snd

apply (BuiltIn Concat : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 2
        , foundCount    = length arguments
        , functionName  = Just "concat"
        }

apply [BuiltIn Split, first, second] = do
    on     <- eval first
    string <- eval second
    throwIfError $ String.split on string

apply (BuiltIn Split : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 2
        , foundCount    = length arguments
        , functionName  = Just "split"
        }

{-  Type operations
    The ones that check if a thing has a type -}

apply [BuiltIn IsFunction, argument] = do
    result <- eval argument
    return . Bool $ SExp.toType result == Type.Function

apply (BuiltIn IsFunction : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "function?"
        }

apply [BuiltIn IsBool, argument] = do
    result <- eval argument
    return . Bool $ SExp.toType result == Type.Bool

apply (BuiltIn IsBool : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "bool?"
        }

apply [BuiltIn IsError, argument] = do
    result <- eval argument
    return . Bool $ SExp.toType result == Type.Error

apply (BuiltIn IsError : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "error?"
        }

apply [BuiltIn IsSymbol, argument] = do
    result <- eval argument
    return . Bool $ SExp.toType result == Type.Symbol

apply (BuiltIn IsSymbol : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "symbol?"
        }

apply [BuiltIn IsMacro, argument] = do
    result <- eval argument
    return . Bool $ SExp.toType result == Type.Macro

apply (BuiltIn IsMacro : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "macro?"
        }

apply [BuiltIn IsString, argument] = do
    result <- eval argument
    return . Bool $ SExp.toType result == Type.String

apply (BuiltIn IsString : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "string"
        }

apply [BuiltIn IsNumber, argument] = do
    result <- eval argument
    return . Bool $ SExp.toType result == Type.Number

apply (BuiltIn IsNumber : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "number?"
        }

apply [BuiltIn IsPair, argument] = do
    result <- eval argument
    return . Bool $ SExp.toType result == Type.Pair

apply (BuiltIn IsPair : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "pair?"
        }

{- Error handling
   raise, call-with-error-handler -}

apply [BuiltIn Raise, code, message] = do
    case (code, message) of
        (Quoted (Symbol code), String message) ->
            throw @"runtimeError" $ UserRaised code message
        (Quoted (Symbol code), invalidMessage) ->
            throw @"runtimeError" $ TypeMismatch
                { expected = Simple Type.String
                , found    = SExp.toType invalidMessage
                }
        (invalidCode, _) -> throw @"runtimeError" $ TypeMismatch
            { expected = Simple Type.QuotedSymbol
            , found    = SExp.toType invalidCode
            }

apply (BuiltIn Raise : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 2
        , foundCount    = length arguments
        , functionName  = Just "raise"
        }

apply [BuiltIn CallWithErrorHandler, function, handler] =
    catch @"runtimeError" (eval function) (return . Error) >>= \case
        error@(Error _) -> apply [handler, error]
        success         -> return success

apply (BuiltIn CallWithErrorHandler : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 2
        , foundCount    = length arguments
        , functionName  = Just "call-with-error-handler"
        }

apply [BuiltIn ErrorCode, argument] = eval argument >>= \case
    Error error -> return . Symbol $ Error.code error
    invalid     -> throw @"runtimeError" $ TypeMismatch
        { expected = Simple Type.Error
        , found    = SExp.toType invalid
        }

apply (BuiltIn ErrorCode : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "error-code"
        }

{- Import operations -}

apply [BuiltIn Import, argument] = case Import.getInformation argument of
    Just ImportInformation { prefix, kind } -> do
        currentPath <- ask @"executionPath"
        newPath <- liftIO (Import.getNewPath currentPath kind) >>= throwIfError

        fileContents <-
            (liftIO . Import.safelyReadFile $ newPath) >>= throwIfError
        functions <- throwIfError (first ParserError $ parseFile fileContents)
        moduleEnvironment <- liftIO
            $ loadModule (takeDirectory newPath) functions
        Environment.merge (fromMaybe "" prefix) moduleEnvironment
        return (Pair Nil)

    Nothing -> throw @"runtimeError" $ TypeMismatch
        { expected = Multiple
                         (Type.String :| [Type.Symbol, Type.ImportPrefixWith])
        , found    = SExp.toType argument
        }

apply (BuiltIn Import : arguments) =
    throw @"runtimeError" $ WrongNumberOfArguments
        { expectedCount = 1
        , foundCount    = length arguments
        , functionName  = Just "import"
        }


{-  Remaining operations
    evaluating symbols, lists, etc.
-}

apply (s@(Symbol _) : as) = do
    operator <- eval s
    apply (operator : as)

apply (Pair (List l) : as) = do
    operator <- apply l
    apply (operator : as)

apply (Pair   (_ :.: _) : _) = throw @"runtimeError" $ CannotApply Type.Pair

apply (Pair   Nil       : _) = throw @"runtimeError" $ CannotApply Type.Pair

apply (Bool   _         : _) = throw @"runtimeError" $ CannotApply Type.Bool

apply (String _         : _) = throw @"runtimeError" $ CannotApply Type.String

apply (Number _         : _) = throw @"runtimeError" $ CannotApply Type.Number

apply (e@(Error _)      : _) = return e

loadModule :: FilePath -> [SExp.T] -> IO (Environment.T SExp.T)
loadModule directory code = do
    environment <- basicEnvironment directory
    mapM_ (\instruction -> Environment.runM (eval instruction) environment) code
    return environment
