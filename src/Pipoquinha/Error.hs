module Pipoquinha.Error where

import           GHC.Show                       ( Show(..) )
import qualified Pipoquinha.Type               as Type
import           Protolude               hiding ( show )

data T
  = UndefinedVariable Text
  | TypeMismatch { expected :: Type.T
                 , found    :: Type.T
                 }
  | NestedMultiArityFunction
  | MultipleVariadicFunction
  | InvalidFunctionBody
  | OverlappingBodies
  | WrongNumberOfArguments { expectedCount :: Int
                           , foundCount    :: Int
                           , functionName :: Maybe Text
                           }
  | NotEnoughArguments { expectedCount :: Int
                       , foundCount :: Int
                       , functionName :: Maybe Text
                       }
  | NoCompatibleBodies (Maybe Text)
  | ParserError Text
  | CannotApply Type.T
  | NotImplementedYet
  | MalformedDefinition
  | MalformedSet
  | MalformedLet
  | NotParameterList
  | RepeatedParameter
  | MisplacedVariadic
  | MalformedCond
  | DividedByZero

    deriving (Eq, Ord)

instance Exception T

instance Show T where
  show (UndefinedVariable name) = "Undefined variable: " <> toS name
  show TypeMismatch { expected, found } =
    "Type mismatch: expecting " <> show expected <> ", found " <> show found
  show (ParserError error)      = "Parser error:\n" <> toS error
  show NestedMultiArityFunction = "Multi-arity functions cannot be nested"
  show MultipleVariadicFunction =
    "You can only have one variadic case in a multi arity function"
  show InvalidFunctionBody = "Invalid function body"
  show OverlappingBodies = "Repeated parameter count in a multi arity function"
  show WrongNumberOfArguments { expectedCount, foundCount, functionName } =
    "Wrong number of arguments for "
      <> maybe "anonymous function" toS functionName
      <> ": expecting "
      <> show expectedCount
      <> ", found "
      <> show foundCount
  show NotEnoughArguments { expectedCount, foundCount, functionName } =
    "Not enough arguments for "
      <> maybe "anonymous function" toS functionName
      <> ": expecting at least "
      <> show expectedCount
      <> ", found "
      <> show foundCount
  show (NoCompatibleBodies name) =
    "No compatible body found with this number of arguments for "
      <> maybe "anonymous function" toS name
  show (CannotApply dataType) =
    "Cannot apply " <> show dataType <> " as a function"
  show NotImplementedYet = "Not implemented yet"
  show MalformedDefinition =
    "Malformed definition found. Variable and function definitions should be written as `def` followed by a symbol and a value, like:\n"
      <> "(def name 50) -- for variables; or \n"
      <> "(defn inc (x) (+ x 1)) -- for functions."
  show MalformedSet
    = "Malformed set! found. The `set` built-in should be used by providing the variable's name as a symbol and its new value, for example: (set! x 10)"
  show MalformedLet = "Malformed let found."
  show NotParameterList
    = "Malformed parameter list found. A function's parameter list should only have symbols."
  show RepeatedParameter
    = "Non-unique parameter name found. A function cannot have two parameters with the same name."
  show MisplacedVariadic
    = "Misplaced variadic parameter found. The +rest parameter is a special syntax for variadics, and show be placed by the end of the parameter list."
  show MalformedCond = "Malformed cond found."
  show DividedByZero = "Divided by zero"
