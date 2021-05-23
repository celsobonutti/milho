{-# LANGUAGE ViewPatterns #-}

module Canjica.Function where

import Canjica.State
import Capability.State
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Sequence (Seq((:|>)))
import qualified Data.Set as Set
import Data.Text (Text)
import Pipoquinha.Types.Data
import Protolude

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
  Right $ Simple { body, parameters, scope }
create scope [validateParameters -> VariadicP parameters, body] =
  Right $ Variadic { body, parameters, scope }
create scope [validateParameters -> Invalid reason, body] =
  Left $ case reason of
    NotListOfSymbols -> "Parameter list must be a list of Symbols"
    RepeatedSymbol -> "There must be no repeated parameter names"
    MisplacedVariadic -> "The parameter is special and must be at the end of the list, representing a variadic function"
create _ _ = Left "Invalid function definition: a function definition should be formed by a list of symbols and the body of the function."

validateParameters :: Atom -> FunctionParameters
validateParameters (List atoms)
  | not onlyHasSymbols = Invalid NotListOfSymbols
  | not isUniq =  Invalid RepeatedSymbol
  | otherwise =
    case Seq.fromList . mapMaybe extractName $ atoms of
      (parameters :|> "+rest") ->
        VariadicP parameters
      (("+rest" `elem`) -> True) ->
        Invalid MisplacedVariadic
      parameters ->
        SimpleP parameters
  where
    onlyHasSymbols = all isSymbol atoms
    isUniq = (length === Set.size . Set.fromList) atoms
    extractName (Symbol s) = Just s
    extractName _ = Nothing
validateParameters _ = Invalid NotListOfSymbols

proceed :: Fun -> [Atom] -> (VarTable, Atom)
proceed Simple {body, parameters} arguments
  | length arguments == length parameters = (localTable, body)
  | otherwise =
    (Map.empty, Error "Wrong number of arguments for function")
  where
    localTable = Map.fromList (zip (toList parameters) arguments)
proceed Variadic {body, parameters} arguments
  | length arguments >= length parameters = (variadicTable, body)
  | otherwise =
    (Map.empty, Error "Wrong number of arguments for function")
  where
    (nonVariadic, rest) = splitAt (length parameters) arguments
    localTable = Map.fromList (zip (toList parameters) nonVariadic)
    variadicTable = Map.insert "+rest" (List rest) localTable
proceed _ _ =
  (Map.empty, Error "oops")

infixr 9 ===
(===) :: Eq b => (a -> b) -> (a -> b) -> a -> Bool
(===) = liftA2 (==)


