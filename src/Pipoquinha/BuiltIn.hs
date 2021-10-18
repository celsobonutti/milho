module Pipoquinha.BuiltIn where

import           GHC.Show                       ( Show(..) )
import           Protolude

data T
  = Add
  | Mul
  | Negate
  | Invert
  | Eql
  | Defn
  | Defmacro
  | Def
  | Fn
  | Let
  | If
  | Read
  | Eval
  | PrintLn
  | Print
  | Loop
  | Do
  | Cons
  | Car
  | Cdr
  | Quote
  | Gt
  | Lt
  | Numerator
  | Set
  | IsFunction
  | IsBool
  | IsError
  | IsSymbol
  | IsMacro
  | IsString
  | IsNumber
  | IsPair
  | Concat
  | Str
  | Split
  | Cond
  | Raise
  | CallWithErrorHandler
  | ErrorCode

  deriving (Eq, Ord, Enum, Bounded)

instance Show T where
  show Add                  = "add"
  show Mul                  = "mul"
  show Negate               = "negate"
  show Invert               = "invert"
  show Eql                  = "eq"
  show Defn                 = "defn"
  show Defmacro             = "defmacro"
  show Def                  = "def"
  show Fn                   = "fn"
  show Let                  = "let"
  show If                   = "if"
  show Cond                 = "cond"
  show Read                 = "read"
  show Eval                 = "eval"
  show PrintLn              = "println"
  show Print                = "print"
  show Loop                 = "loop"
  show Do                   = "do"
  show Cons                 = "cons"
  show Car                  = "car"
  show Cdr                  = "cdr"
  show Quote                = "quote"
  show Gt                   = "gt"
  show Lt                   = "lt"
  show Numerator            = "numerator"
  show Set                  = "set"
  show Concat               = "concat"
  show IsString             = "string?"
  show Str                  = "str"
  show Split                = "split"
  show IsFunction           = "function?"
  show IsBool               = "bool?"
  show IsError              = "error?"
  show IsSymbol             = "symbol?"
  show IsMacro              = "macro?"
  show IsNumber             = "number?"
  show IsPair               = "pair?"
  show Raise                = "raise"
  show CallWithErrorHandler = "call-with-error-handler"
  show ErrorCode            = "error-code"
