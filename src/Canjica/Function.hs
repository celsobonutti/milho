module Canjica.Function where

import           Capability.Error        hiding ( (:.:) )
import           Capability.State        hiding ( (:.:) )
import           Data.Containers.ListUtils      ( nubOrdOn )
import           Data.IORef
import           Data.List                      ( partition )
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq((:|>)) )
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Pipoquinha.Environment        as Environment
import           Pipoquinha.Environment         ( ThrowCapable )
import qualified Pipoquinha.Error              as Error
import           Pipoquinha.Error               ( T(WrongNumberOfArguments) )
import qualified Pipoquinha.Function           as Function
import           Pipoquinha.Function
import           Pipoquinha.SExp         hiding ( T )
import qualified Pipoquinha.SExp               as SExp
import           Protolude

data ArgumentError
  = NotListOfSymbols
  | RepeatedSymbol
  | MisplacedVariadic

data FunctionParameters
  = SimpleP (Seq Text)
  | VariadicP (Seq Text)
  | Invalid ArgumentError

make
  :: Environment.ThrowCapable m
  => Environment.Table SExp.T
  -> [SExp.T]
  -> m Function
make environment [validateParameters -> SimpleP parameters, body] =
  return $ Simple SF { body, parameters, environment }

make environment [validateParameters -> VariadicP parameters, body] =
  return $ Variadic VF { body, parameters, environment }

make environment (fmap (fmap splitFunctions  . traverse (make environment)) . traverse fromSExpList -> Just (Right functions))
  = case functions of
    (_, _, multiArityFunction : _) ->
      throw @"runtimeError" Error.NestedMultiArityFunction

    (_, _ : _ : _, []) -> throw @"runtimeError" Error.MultipleVariadicFunction

    (simpleFunctions, head -> variadic, []) ->
      let bodies =
            Map.fromList
              . map (\x@SF { parameters } -> (length parameters, x))
              $ simpleFunctions
      in  if Map.size bodies == length simpleFunctions
            then return $ MultiArity MAF { bodies, variadic }
            else throw @"runtimeError" Error.OverlappingBodies

make environment [validateParameters -> Invalid reason, body] =
  throw @"runtimeError" $ case reason of
    NotListOfSymbols  -> Error.NotListOfSymbols
    RepeatedSymbol    -> Error.RepeatedSymbol
    MisplacedVariadic -> Error.MisplacedVariadic

make _ _ = throw @"runtimeError" Error.InvalidFunctionBody

splitFunctions
  :: [Function]
  -> ( [Function.Simple SExp.T]
     , [Function.Variadic SExp.T]
     , [Function.MultiArity SExp.T]
     )
splitFunctions = foldr go ([], [], [])
 where
  go (Simple f@SF{}) (simple, variadics, multiArity) =
    (f : simple, variadics, multiArity)
  go (Variadic f@VF{}) (simple, variadics, multiArity) =
    (simple, f : variadics, multiArity)
  go (MultiArity f@MAF{}) (simple, variadics, multiArity) =
    (simple, variadics, f : multiArity)

fromSExpList :: SExp.T -> Maybe [SExp.T]
fromSExpList (Pair (List f@[Pair (List _), _])) = Just f
fromSExpList _ = Nothing

validateParameters :: SExp.T -> FunctionParameters

validateParameters (Pair (List atoms))
  | not onlyHasSymbols = Invalid NotListOfSymbols
  | not isUniq = Invalid RepeatedSymbol
  | otherwise = case Seq.fromList . mapMaybe extractName $ atoms of
    (parameters :|> "+rest")               -> VariadicP parameters
    parameters | "+rest" `elem` parameters -> Invalid MisplacedVariadic
    parameters                             -> SimpleP parameters
 where
  onlyHasSymbols = all isSymbol atoms
  isUniq         = (length === Set.size . Set.fromList) atoms
  extractName (Symbol s) = Just s
  extractName _          = Nothing

validateParameters _ = Invalid NotListOfSymbols

proceed
  :: ThrowCapable m
  => [SExp.T]
  -> Function
  -> m (Map Text SExp.T, Environment, SExp.T)

proceed arguments (Simple SF { body, parameters, environment })
  | length arguments == length parameters = return
    (localTable, environment, body)
  | otherwise = throw @"runtimeError"
    (Error.WrongNumberOfArguments (length arguments) (length parameters))
  where localTable = Map.fromList (zip (toList parameters) arguments)

proceed arguments (Variadic VF { body, parameters, environment })
  | length arguments >= length parameters = return
    (localTable, environment, body)
  | otherwise = throw @"runtimeError"
    (Error.NotEnoughArguments (length arguments) (length parameters))
 where
  (nonVariadic, rest) = splitAt (length parameters) arguments
  localTable          = Map.fromList (zip (toList parameters) nonVariadic)
  variadicTable       = Map.insert "+rest" (Pair (List rest)) localTable

proceed arguments (MultiArity MAF { bodies, variadic }) = _

infixr 9 ===
(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(===) = liftA2 (==)
