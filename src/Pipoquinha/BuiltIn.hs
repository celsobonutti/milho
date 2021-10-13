module Pipoquinha.BuiltIn where

import           Protolude

data T
  = Add
  | Mul
  | Negate
  | Invert
  | Eql
  | Def
  | Defn
  | Defmacro
  | Fn
  | Let
  | If
  | Read
  | Eval
  | Print
  | Loop
  | Do
  | Not
  | Cons
  | MakeList
  | Car
  | Cdr
  | Quote
  | Gt
  | Numerator
  | Set
  deriving (Eq, Show, Ord)
