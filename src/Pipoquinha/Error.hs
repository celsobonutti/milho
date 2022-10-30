module Pipoquinha.Error where

import           Data.List.NonEmpty
import           GHC.Show                       ( Show(..) )
import qualified Pipoquinha.Type               as Type
import           MilhoPrelude               hiding ( show )
import Data.Text
import Capability.Error (Exception)

data ExpectedType
  = Simple Type.T
  | Multiple (NonEmpty Type.T)
  deriving (Eq, Ord)

instance Show ExpectedType where
  show (Simple   t            ) = show t
  show (Multiple (t :| []    )) = show t
  show (Multiple (t :| [last])) = show t <> ", or " <> show last
  show (Multiple (t :| rest)) =
    show t <> ", " <> (show . Multiple . fromList $ rest)

data T
  = UndefinedVariable Text
  | TypeMismatch { expected :: ExpectedType
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
  | MalformedDefinition
  | MalformedSet
  | MalformedLet
  | NotParameterList
  | RepeatedParameter
  | MisplacedVariadic
  | DividedByZero
  | FileError Text
  | CyclicImport { imported :: FilePath, importing :: FilePath }
  | UserRaised { errorCode :: Text, message :: Text }

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
  show DividedByZero = "Divided by zero"
  show UserRaised { errorCode, message } =
    "Error code: " <> toS errorCode <> "\n" <> toS message
  show (FileError path) =
    "Error reading file '"
      <> toS path
      <> "'. Are you sure it exists and you have reading permission?"
  show CyclicImport { importing, imported } =
    "Cyclic import found in " <> importing <> " while importing " <> imported

code :: T -> Text
code (UndefinedVariable _)    = "undefined-variable"
code TypeMismatch{}           = "type-mismatch"
code NestedMultiArityFunction = "nested-multi-arity"
code MultipleVariadicFunction = "multiple-variadic"
code InvalidFunctionBody      = "invalid-body"
code OverlappingBodies        = "overlapping-body"
code WrongNumberOfArguments{} = "wrong-number-of-arguments"
code NotEnoughArguments{}     = "not-enough-arguments"
code (NoCompatibleBodies _)   = "no-compatible-bodies"
code (ParserError        _)   = "parse-error"
code (CannotApply        _)   = "cannot-apply"
code MalformedDefinition      = "malformed-definition"
code MalformedSet             = "malformed-set"
code MalformedLet             = "malformed-let"
code NotParameterList         = "not-parameter-list"
code RepeatedParameter        = "repeated-parameter"
code MisplacedVariadic        = "misplaced-variadic"
code DividedByZero            = "divided-by-zero"
code UserRaised { errorCode } = errorCode
code (FileError _)            = "file-error"
code CyclicImport{}           = "cyclic-import"
