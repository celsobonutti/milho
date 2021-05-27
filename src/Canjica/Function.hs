{-# LANGUAGE ViewPatterns #-}

module Canjica.Function where

import Canjica.State
import Capability.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:|>)))
import Data.Containers.ListUtils (nubOrdOn)
import qualified Data.Set as Set
import Data.Text (Text)
import Pipoquinha.Types.Data
import qualified Pipoquinha.Types.Pair as Pair
import Protolude
import Data.List (partition)

data ArgumentError
  = NotListOfSymbols
  | RepeatedSymbol
  | MisplacedVariadic

data FunctionParameters
  = SimpleP (Seq Text)
  | VariadicP (Seq Text)
  | Invalid ArgumentError

create :: VarTable -> [Atom] -> Either Text Fun
create scope [validateParameters -> SimpleP parameters, body] =
  Right . Simple $ SF { body, parameters, scope }
create scope [validateParameters -> VariadicP parameters, body] =
  Right . Variadic $ VF { body, parameters, scope }
create scope (fmap (fmap splitFunctions  . traverse (create scope)) . traverse fromAtomList -> Just (Right functions)) =
  case functions of
    (_, _, multiArityFunction : _) -> Left "Cannot have a multi arity function inside another on"
    (_, _ : _ : _, []) -> Left "Cannot have more than one variadic function body"
    (simpleFunctions, head -> variadic, []) ->
      let
        bodies = Map.fromList . map (\x@SF{parameters} -> (length parameters, x)) $ simpleFunctions
      in
        if Map.size bodies == length simpleFunctions then
          Right . MultiArity  $ MAF {bodies, variadic}
        else
          Left "Cannot have multiple bodies with the same number of arguments"
create scope [validateParameters -> Invalid reason, body] =
  Left $ case reason of
    NotListOfSymbols -> "Parameter list must be a list of Symbols"
    RepeatedSymbol -> "There must be no repeated parameter names"
    MisplacedVariadic -> "The parameter is special and must be at the end of the list, representing a variadic function"
create _ _ =
  Left "Invalid function body"

splitFunctions :: [Fun] -> ([SimpleFunction], [VariadicFunction], [MultiArityFunction])
splitFunctions = foldr go ([], [], [])
  where
    go (Simple f@SF{}) (simple, variadics, multiArity)  = (f : simple, variadics, multiArity)
    go (Variadic f@VF{}) (simple, variadics, multiArity)  = (simple, f : variadics, multiArity)
    go (MultiArity f@MAF{}) (simple, variadics, multiArity)  = (simple, variadics, f : multiArity)

fromAtomList :: Atom -> Maybe [Atom]
fromAtomList (Pair (List f@[Pair (List _), _])) =
  Just f
fromAtomList _ =
  Nothing

validateParameters :: Atom -> FunctionParameters
validateParameters (Pair (List atoms))
  | not onlyHasSymbols = Invalid NotListOfSymbols
  | not isUniq =  Invalid RepeatedSymbol
  | otherwise =
    case Seq.fromList . mapMaybe extractName $ atoms of
      (parameters :|> "+rest") ->
        VariadicP parameters
      parameters | "+rest" `elem` parameters ->
        Invalid MisplacedVariadic
      parameters ->
        SimpleP parameters
  where
    onlyHasSymbols = all isSymbol atoms
    isUniq = (length === Set.size . Set.fromList) atoms
    extractName (Symbol s) = Just s
    extractName _ = Nothing
validateParameters _ = Invalid NotListOfSymbols

proceed :: [Atom] -> Fun -> (VarTable, Atom)
proceed  arguments (Simple SF {body, parameters, scope})
  | length arguments == length parameters = (Map.union localTable scope , body)
  | otherwise =
    (Map.empty, Error "Wrong number of arguments for function")
  where
    localTable = Map.fromList (zip (toList parameters) arguments)
proceed arguments (Variadic VF {body, parameters, scope})
  | length arguments >= length parameters = (Map.union variadicTable scope, body)
  | otherwise =
    (Map.empty, Error "Not enough arguments for variadic function")
  where
    (nonVariadic, rest) = splitAt (length parameters) arguments
    localTable = Map.fromList (zip (toList parameters) nonVariadic)
    variadicTable = Map.insert "+rest" (Pair (List rest)) localTable
proceed  arguments (MultiArity MAF {bodies, variadic}) =
  case proceed arguments . Simple <$> Map.lookup (length arguments) bodies of
    Nothing ->
      case proceed arguments . Variadic <$> variadic of
        Nothing -> (Map.empty, Error "No body matches this number of arguments")
        Just (_, Error _) -> (Map.empty, Error "No body matches this number of arguments")
        Just res -> res

    Just result -> result

infixr 9 ===
(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(===) = liftA2 (==)


