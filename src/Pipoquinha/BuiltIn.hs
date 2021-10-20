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
  | Type
  | Concat
  | Str
  | Split
  | Raise
  | CallWithErrorHandler
  | ErrorCode
  | Import

  deriving (Eq, Ord, Enum, Bounded)

instance Show T where
  show Add                  = "+"
  show Mul                  = "*"
  show Negate               = "negate"
  show Invert               = "invert"
  show Eql                  = "eq?"
  show Defn                 = "defn"
  show Defmacro             = "defmacro"
  show Def                  = "def"
  show Fn                   = "fn"
  show Let                  = "let"
  show If                   = "if"
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
  show Gt                   = ">"
  show Lt                   = "<"
  show Numerator            = "numerator"
  show Set                  = "set!"
  show Type                 = "type"
  show Concat               = "concat"
  show Str                  = "str"
  show Split                = "split"
  show Raise                = "raise"
  show CallWithErrorHandler = "call-with-error-handler"
  show ErrorCode            = "error-code"
  show Import               = "import"
