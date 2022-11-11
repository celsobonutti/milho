module Canjica.EvalApply where

import qualified Canjica.Boolean as Boolean
import Canjica.Function (
    ProceedResult (..),
    functionArguments,
    functionBody,
    functionEnvironment,
 )
import qualified Canjica.Function as Function
import Canjica.Import (ImportInformation (..))
import qualified Canjica.Import as Import
import qualified Canjica.Let as Let
import qualified Canjica.List as List
import qualified Canjica.Macro as Macro
import qualified Canjica.Number as Number
import qualified Canjica.String as String
import Capability.Error hiding (
    catchJust,
    (:.:),
 )
import Capability.Reader hiding ((:.:))
import Capability.State hiding ((:.:))
import Control.Exception.Base (runtimeError)
import Control.Monad (foldM, forever, join, (>=>))
import Data.Bifunctor (first)
import Data.Functor ((<&>))
import Data.IORef
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import Data.Text (pack, unpack)
import MilhoPrelude
import qualified MilhoPrelude
import Pipoquinha.BuiltIn hiding (T)
import Pipoquinha.Environment (
    CatchCapable,
    ReaderCapable,
    StateCapable,
    ThrowCapable,
 )
import qualified Pipoquinha.Environment as Environment
import Pipoquinha.Error (
    ExpectedType (..),
    T (..),
 )
import qualified Pipoquinha.Error as Error
import qualified Pipoquinha.ImportStack as ImportStack
import Pipoquinha.Parser as Parser
import Pipoquinha.SExp hiding (T)
import qualified Pipoquinha.SExp as SExp
import qualified Pipoquinha.Type as Type
import System.Directory (makeAbsolute)
import System.FilePath (dropFileName)

eval ::
    (ReaderCapable SExp.T m, CatchCapable m, StateCapable SExp.T m) =>
    SExp.T ->
    m SExp.T
eval atom = case atom of
    n@(Number _) -> return n
    f@(Function _) -> return f
    m@(Macro _) -> return m
    b@(Bool _) -> return b
    e@(Error _) -> return e
    s@(String _) -> return s
    b@(BuiltIn _) -> return b
    Symbol name -> do
        join (asks @"table" (Environment.getValue name)) >>= liftIO . readIORef
    Pair (List l) -> apply l
    Pair (_ :.: _) -> throw @"runtimeError" $ CannotApply Type.Pair
    Pair _ -> return $ Pair Nil

batchEval ::
    (ReaderCapable SExp.T m, CatchCapable m, StateCapable SExp.T m) =>
    [SExp.T] ->
    m [SExp.T]
batchEval = traverse eval

apply ::
    (ReaderCapable SExp.T m, CatchCapable m, StateCapable SExp.T m) =>
    [SExp.T] ->
    m SExp.T
apply [] = return $ Pair Nil
{- Comparison operations -}

apply (BuiltIn Eql : arguments) =
    batchEval arguments >>= \case
        [] -> throwNotEnoughArguments 1 arguments "eq?"
        (base : rest) -> return (Bool $ (== base) `all` rest)
apply (BuiltIn Gt : arguments) =
    batchEval arguments >>= \case
        [] -> throwNotEnoughArguments 1 arguments ">"
        arguments -> do
            isGreater <-
                throwIfError
                    (mapM (uncurry Number.gt) (List.pairs arguments))
            throwIfError (foldM Boolean.and (Bool True) isGreater)

{- Number operations -}

apply (BuiltIn Add : arguments) =
    batchEval arguments <&> foldM Number.add (Number 0) >>= throwIfError
apply (BuiltIn Mul : arguments) =
    batchEval arguments <&> foldM Number.mul (Number 1) >>= throwIfError
apply [BuiltIn Negate, atom] =
    eval atom >>= \case
        Number x -> return . Number . negate $ x
        value ->
            throw @"runtimeError"
                TypeMismatch
                    { expected = Simple Type.Number
                    , found = SExp.toType value
                    }
apply (BuiltIn Negate : arguments) =
    throwWrongNoOfArguments 1 arguments "negate"
apply [BuiltIn Invert, atom] =
    eval atom >>= \case
        Number 0 -> throw @"runtimeError" DividedByZero
        Number x -> return . Number $ denominator x % numerator x
        value ->
            throw @"runtimeError"
                TypeMismatch
                    { expected = Simple Type.Number
                    , found = SExp.toType value
                    }
apply (BuiltIn Invert : arguments) =
    throwWrongNoOfArguments 1 arguments "invert"
apply [BuiltIn Numerator, atom] =
    eval atom >>= \case
        Number x -> return . Number $ numerator x % 1
        value ->
            throw @"runtimeError"
                TypeMismatch
                    { expected = Simple Type.Number
                    , found = SExp.toType value
                    }
apply (BuiltIn Numerator : arguments) =
    throwWrongNoOfArguments 1 arguments "numerator"
{- Control flow operations -}

apply [BuiltIn If, predicate, consequent, alternative] =
    eval predicate >>= \case
        Bool False -> eval alternative
        _ -> eval consequent
apply (BuiltIn If : arguments) = throwWrongNoOfArguments 3 arguments "if"
{-  Variable definition/mutation operations
    def, defn, defmacro, set!, let, guard   -}

apply [BuiltIn Def, Symbol name, atom] = do
    evaluatedSExp <- eval atom
    environment <- ask @"table"
    Environment.insertValue name evaluatedSExp
    return (Symbol name)
apply (BuiltIn Def : arguments) = throw @"runtimeError" $ MalformedDefinition
apply (BuiltIn Defn : Symbol name : rest) = do
    environment <- ask @"table"
    function <- throwIfError $ Function.make environment (Just name) rest
    Environment.insertValue name (Function function)
    return $ Symbol name
apply (BuiltIn Defn : arguments) = throw @"runtimeError" $ MalformedDefinition
apply (BuiltIn Defmacro : Symbol name : rest) = do
    environment <- ask @"table"
    function <- throwIfError $ Function.make environment (Just name) rest
    Environment.insertValue name (Macro function)
    return $ Symbol name
apply (BuiltIn Defmacro : arguments) =
    throw @"runtimeError" $ MalformedDefinition
apply [BuiltIn Set, Symbol name, atom] = do
    evaluatedSExp <- eval atom
    environment <- ask @"table"
    Environment.setValue name evaluatedSExp environment
    return (Symbol name)
apply (BuiltIn Set : _) = throw @"runtimeError" MalformedSet
apply [BuiltIn Let, Pair (List exps), body] = do
    letTable <- throwIfError $ Let.makeTable exps

    environment <- ask @"table"

    evaluatedArguments <- mapM eval letTable

    newScope <- liftIO . mapM newIORef $ evaluatedArguments

    environmentRef <-
        liftIO . newIORef $
            Environment.Table
                { variables = newScope
                , parent = Just environment
                }

    local @"table" (const environmentRef) (eval body)
apply (BuiltIn Let : arguments) = throwWrongNoOfArguments 2 arguments "let"
{-  Function/macro operations
    fn, funcion application and macro expansion -}

apply (BuiltIn Fn : rest) = do
    environment <- ask @"table"
    function <- throwIfError $ Function.make environment Nothing rest
    return $ Function function
apply (Function fn : values) = do
    evaluated <- batchEval values
    ProceedResult{functionArguments, functionEnvironment, functionBody} <-
        throwIfError $ Function.proceed fn evaluated

    newScope <- liftIO . mapM newIORef $ functionArguments

    environmentRef <-
        liftIO . newIORef $
            Environment.Table
                { variables = newScope
                , parent = Just functionEnvironment
                }

    local @"table" (const environmentRef) (eval functionBody)
apply (Macro macro : values) = do
    ProceedResult{functionArguments, functionEnvironment, functionBody} <-
        throwIfError $ Function.proceed macro values

    let expanded = Macro.expand functionArguments functionBody

    eval expanded

{-  REPL
    Read, Eval, Print, Loop -}

apply [BuiltIn Read] =
    liftIO getLine >>= \case
        "" -> apply [BuiltIn Read]
        input -> return $ parseExpression input
apply (BuiltIn Read : arguments) = throwWrongNoOfArguments 0 arguments "read"
apply [BuiltIn Eval, value] = eval >=> eval $ value
apply (BuiltIn Eval : arguments) = throwWrongNoOfArguments 1 arguments "eval"
apply (BuiltIn PrintLn : rest) =
    batchEval rest
        >>= liftIO . putStrLn . unwords . map showUnlessString
        >> return
            (Pair Nil)
  where
    showUnlessString (String s) = s
    showUnlessString other = show other
apply (BuiltIn Print : rest) =
    batchEval rest
        >>= liftIO . putStr . unwords . map showUnlessString
        >> return
            (Pair Nil)
  where
    showUnlessString (String s) = s
    showUnlessString other = show other
apply [BuiltIn Loop, procedure] = forever (eval procedure)
apply (BuiltIn Loop : arguments) = throwWrongNoOfArguments 1 arguments "loop"
apply [BuiltIn Quote, atom] = return atom
apply (BuiltIn Quote : arguments) = throwWrongNoOfArguments 1 arguments "quote"
apply (BuiltIn Do : rest) = foldM (const eval) (Pair Nil) rest
{-  List operations
    cons, car, cdr -}

apply [BuiltIn Cons, car, cdr] = do
    car <- eval car
    cdr <- eval cdr
    return $ Pair (car :.: cdr)
apply (BuiltIn Cons : arguments) = throwWrongNoOfArguments 2 arguments "cons"
apply [BuiltIn Car, atom] =
    eval atom >>= \case
        Pair Nil -> return $ Pair Nil
        Pair (x ::: _) -> return x
        Pair (x :.: _) -> return x
        value ->
            throw @"runtimeError"
                TypeMismatch
                    { expected = Simple Type.Pair
                    , found = SExp.toType value
                    }
apply (BuiltIn Car : arguments) = throwWrongNoOfArguments 1 arguments "car"
apply [BuiltIn Cdr, atom] =
    eval atom >>= \case
        Pair Nil -> return $ Pair Nil
        Pair (_ ::: xs) -> return $ Pair xs
        Pair (_ :.: x) -> return x
        value ->
            throw @"runtimeError"
                TypeMismatch
                    { expected = Simple Type.Pair
                    , found = SExp.toType value
                    }
apply (BuiltIn Cdr : arguments) = throwWrongNoOfArguments 1 arguments "cdr"
{-  String operations
    str, concat, split -}

apply [BuiltIn Str, argument] =
    eval argument >>= \case
        String s -> return $ String s
        value -> return . String . show $ value
apply (BuiltIn Str : arguments) = throwWrongNoOfArguments 1 arguments "str"
apply [BuiltIn Concat, first, second] = do
    fst <- eval first
    snd <- eval second
    throwIfError $ String.concat fst snd
apply (BuiltIn Concat : arguments) =
    throwWrongNoOfArguments 2 arguments "concat"
apply [BuiltIn Split, first, second] = do
    on <- eval first
    string <- eval second
    throwIfError $ String.split on string
apply (BuiltIn Split : arguments) = throwWrongNoOfArguments 2 arguments "split"
{-  Type operations
    The ones that check if a thing has a type -}

apply [BuiltIn Type, argument] = eval argument <&> Symbol . show . SExp.toType
apply (BuiltIn Type : arguments) = throwWrongNoOfArguments 1 arguments "type"
{- Error handling
   raise, call-with-error-handler -}

apply [BuiltIn Raise, code, message] = do
    case (code, message) of
        (Quoted (Symbol code), String message) ->
            throw @"runtimeError" $ UserRaised code message
        (Quoted (Symbol code), invalidMessage) ->
            throw @"runtimeError" $
                TypeMismatch
                    { expected = Simple Type.String
                    , found = SExp.toType invalidMessage
                    }
        (invalidCode, _) ->
            throw @"runtimeError" $
                TypeMismatch
                    { expected = Simple Type.QuotedSymbol
                    , found = SExp.toType invalidCode
                    }
apply (BuiltIn Raise : arguments) = throwWrongNoOfArguments 2 arguments "raise"
apply [BuiltIn CallWithErrorHandler, function, handler] =
    catch @"runtimeError" (eval function) (return . Error) >>= \case
        error@(Error _) -> apply [handler, error]
        success -> return success
apply (BuiltIn CallWithErrorHandler : arguments) =
    throwWrongNoOfArguments 2 arguments "call-with-error-handler"
apply [BuiltIn ErrorCode, argument] =
    eval argument >>= \case
        Error error -> return . Symbol $ Error.code error
        invalid ->
            throw @"runtimeError" $
                TypeMismatch
                    { expected = Simple Type.Error
                    , found = SExp.toType invalid
                    }
apply (BuiltIn ErrorCode : arguments) =
    throwWrongNoOfArguments 1 arguments "error-code"
{- Import operations -}

apply [BuiltIn Import, argument] = case Import.getInformation argument of
    Just ImportInformation{prefix, kind} -> do
        currentFile <- asks @"importStack" ImportStack.current

        newFile <-
            liftIO (Import.getNewPath (dropFileName currentFile) kind)
                >>= throwIfError

        hasCycle <- asks @"importStack" (elem newFile)

        if hasCycle
            then
                throw @"runtimeError" $
                    CyclicImport
                        { importing = currentFile
                        , imported = newFile
                        }
            else do
                stack <- asks @"importStack" (ImportStack.push newFile)

                fileContents <-
                    (liftIO . Import.safelyReadFile $ newFile) >>= throwIfError

                functions <-
                    throwIfError
                        (first ParserError $ parseFile fileContents)

                currentTable <- ask @"table"

                moduleEnvironment <-
                    liftIO $
                        loadModule currentTable stack functions

                Environment.merge (fromMaybe "" prefix) moduleEnvironment

                return (Pair Nil)
    Nothing ->
        throw @"runtimeError" $
            TypeMismatch
                { expected =
                    Multiple
                        (Type.String :| [Type.Symbol, Type.ImportPrefixWith])
                , found = SExp.toType argument
                }
apply (BuiltIn Import : arguments) =
    throwWrongNoOfArguments 1 arguments "import"
{-  Remaining operations
    evaluating symbols, lists, etc.
-}

apply (s@(Symbol _) : as) = do
    operator <- eval s
    apply (operator : as)
apply (Pair (List l) : as) = do
    operator <- apply l
    apply (operator : as)
apply (Pair (_ :.: _) : _) = throw @"runtimeError" $ CannotApply Type.Pair
apply (Pair Nil : _) = throw @"runtimeError" $ CannotApply Type.Pair
apply (Bool _ : _) = throw @"runtimeError" $ CannotApply Type.Bool
apply (String _ : _) = throw @"runtimeError" $ CannotApply Type.String
apply (Number _ : _) = throw @"runtimeError" $ CannotApply Type.Number
apply (e@(Error _) : _) = return e

{- Throw helpers -}

throwIfError :: ThrowCapable m => SExp.Result a -> m a
throwIfError = \case
    Left error -> throw @"runtimeError" error
    Right value -> return value

throwWrongNoOfArguments ::
    ThrowCapable m => Int -> [SExp.T] -> Text -> m SExp.T
throwWrongNoOfArguments expectedCount arguments name =
    throw @"runtimeError" $
        WrongNumberOfArguments
            { expectedCount
            , foundCount = length arguments
            , functionName = Just name
            }

throwNotEnoughArguments ::
    ThrowCapable m => Int -> [SExp.T] -> Text -> m SExp.T
throwNotEnoughArguments expectedCount arguments name =
    throw @"runtimeError" $
        NotEnoughArguments
            { expectedCount
            , foundCount = length arguments
            , functionName = Just name
            }

{- Needs to be here, since it uses `eval` -}

loadModule ::
    Environment.TableRef SExp.T ->
    ImportStack.T ->
    [SExp.T] ->
    IO (Environment.T SExp.T)
loadModule parent importStack code = do
    environment <- Environment.make Map.empty importStack (Just parent)
    mapM_ (\instruction -> Environment.runM (eval instruction) environment) code
    return environment
