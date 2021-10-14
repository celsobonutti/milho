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

make :: Environment.ThrowCapable m => Environment -> [SExp.T] -> m Function
make environment [validateParameters -> SimpleP parameters, body] =
  return $ Simple SF { body, parameters, environment }

make environment [validateParameters -> VariadicP parameters, body] =
  return $ Variadic VF { body, parameters, environment }

make environment parameters = case traverse fromSExpList parameters of
  Nothing -> throw @"runtimeError" Error.InvalidFunctionBody
  Just bodies ->
    mapM (make environment) bodies
      >>= \case
            (_, _, multiArityFunction : _) ->
              throw @"runtimeError" Error.NestedMultiArityFunction

            (_, _ : _ : _, []) ->
              throw @"runtimeError" Error.MultipleVariadicFunction

            (simpleFunctions, variadic, []) ->
              let bodies =
                    Map.fromList
                      . map (\x@SF { parameters } -> (length parameters, x))
                      $ simpleFunctions
              in  if Map.size bodies == length simpleFunctions
                    then return
                      $ MultiArity MAF { bodies, variadic = head variadic }
                    else throw @"runtimeError" Error.OverlappingBodies
      .   splitFunctions

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
  :: (ThrowCapable m, MonadIO m)
  => [SExp.T]
  -> Function
  -> m (Map Text (IORef SExp.T), Environment, SExp.T)

proceed arguments (Simple SF { body, parameters, environment })
  | length arguments == length parameters = do
    arguments <- liftIO $ mapM newIORef arguments
    let newTable = Map.fromList (zip (toList parameters) arguments)
    return (newTable, environment, body)
  | otherwise = throw @"runtimeError"
    (Error.WrongNumberOfArguments (length arguments) (length parameters))

proceed arguments (Variadic VF { body, parameters, environment })
  | length arguments >= length parameters = do
    let (nonVariadic, rest) = splitAt (length parameters) arguments

    nonVariadicArguments <- liftIO $ mapM newIORef nonVariadic
    let table = Map.fromList $ zip (toList parameters) nonVariadicArguments

    variadicArguments <- liftIO . newIORef $ Pair (List rest)
    let newTable = Map.insert "+rest" variadicArguments table

    return (newTable, environment, body)
  | otherwise = throw @"runtimeError"
    (Error.NotEnoughArguments (length arguments) (length parameters))

proceed arguments (MultiArity MAF { bodies, variadic }) =
  case find compatible bodies of
    Just f  -> proceed arguments (Simple f)
    Nothing -> case variadic of
      Just f@VF { parameters } | length arguments >= length parameters ->
        proceed arguments (Variadic f)
      _ -> throw @"runtimeError" Error.NoCompatibleBodies
 where
  compatible :: Function.Simple SExp.T -> Bool
  compatible SF { parameters } = length parameters == length arguments


infixr 9 ===
(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(===) = liftA2 (==)
