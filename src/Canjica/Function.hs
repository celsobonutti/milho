module Canjica.Function
  ( Arguments
  , make
  , proceed
  , ProceedResult(..)
  ) where

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
import           Pipoquinha.Error               ( T(..) )
import qualified Pipoquinha.Function           as Function
import           Pipoquinha.Function
import           Pipoquinha.SExp         hiding ( Result
                                                , T
                                                )
import qualified Pipoquinha.SExp               as SExp
import           Protolude

{-
Function creation functions.

This functions and types are responsible for the validation and creation of functions,
their environments and parameters.
-}

data FunctionParameters
  = SimpleP (Seq Text)
  | VariadicP (Seq Text)
  | Invalid Error.T

make :: Environment -> Maybe Text -> [SExp.T] -> SExp.Result Function
make environment name parameters = case traverse fromSExpList parameters of
  Just bodies -> makeMultiArity environment name bodies

  Nothing     -> case parameters of
    [parameterNames, body] -> case validateParameters parameterNames of

      SimpleP parameters ->
        Right $ Simple SF { body, parameters, environment, name }

      VariadicP parameters ->
        Right $ Variadic VF { body, parameters, environment, name }

      Invalid error -> Left error

    _otherwise -> Left InvalidFunctionBody

validateParameters :: SExp.T -> FunctionParameters
validateParameters (Pair (List atoms))
  | not onlyHasSymbols = Invalid NotParameterList
  | not isUniq = Invalid RepeatedParameter
  | otherwise = case Seq.fromList . mapMaybe extractName $ atoms of
    (parameters :|> "+rest")               -> VariadicP parameters
    parameters | "+rest" `elem` parameters -> Invalid MisplacedVariadic
    parameters                             -> SimpleP parameters
 where
  onlyHasSymbols = all isSymbol atoms
  uniqueNames    = Set.fromList atoms
  isUniq         = length atoms == Set.size uniqueNames
  extractName (Symbol s) = Just s
  extractName _          = Nothing
validateParameters _ = Invalid NotParameterList

makeMultiArity
  :: Environment -> Maybe Text -> [[SExp.T]] -> SExp.Result Function
makeMultiArity environment name bodies =
  mapM (make environment Nothing) bodies
    >>= \case
          (_, _, multiArity) | not (null multiArity) ->
            Left NestedMultiArityFunction

          (_, variadic, []) | length variadic > 1 ->
            Left MultipleVariadicFunction

          (simpleFunctions, variadicList, _) ->
            let makeBody function@SF { parameters } =
                  (length parameters, function)

                bodies   = Map.fromList . map makeBody $ simpleFunctions

                isUnique = Map.size bodies == length simpleFunctions

                variadic = head variadicList
            in  if isUnique
                  then Right $ MultiArity MAF { bodies, variadic, name }
                  else Left OverlappingBodies
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

{-
Function evaluation functions.

These functions and type are responsible for the creation of the environment
in which functions will run.
-}

type Arguments = Map Text SExp.T

data ProceedResult = ProceedResult
  { functionArguments   :: Arguments
  , functionEnvironment :: Environment
  , functionBody        :: SExp.T
  }

proceed :: Function -> [SExp.T] -> SExp.Result ProceedResult

proceed (Simple SF { body, name, parameters, environment }) arguments
  | length arguments == length parameters = do
    let functionArguments = Map.fromList (zip (toList parameters) arguments)

    return $ ProceedResult { functionArguments
                           , functionEnvironment = environment
                           , functionBody        = body
                           }
  | otherwise = Left $ WrongNumberOfArguments
    { expectedCount = length parameters
    , foundCount    = length arguments
    , functionName  = name
    }

proceed (Variadic VF { body, name, parameters, environment }) arguments
  | length arguments >= length parameters
  = let (nonVariadic, variadic) = splitAt (length parameters) arguments
        table = (Map.fromList $ zip (toList parameters) nonVariadic)
        functionArguments = Map.insert "+rest" (Pair (List variadic)) table
    in  Right $ ProceedResult { functionArguments
                              , functionEnvironment = environment
                              , functionBody        = body
                              }
  | otherwise
  = Left $ NotEnoughArguments { expectedCount = length parameters
                              , foundCount    = length arguments
                              , functionName  = name
                              }

proceed (MultiArity MAF { bodies, variadic, name }) arguments =
  case find compatible bodies of
    Just f  -> proceed (Simple f) arguments
    Nothing -> case variadic of
      Just f@VF { parameters } | length arguments >= length parameters ->
        proceed (Variadic f) arguments
      _ -> Left $ NoCompatibleBodies name
 where
  compatible :: Function.Simple SExp.T -> Bool
  compatible SF { parameters } = length parameters == length arguments
