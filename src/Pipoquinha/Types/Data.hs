module Pipoquinha.Types.Data where

import GHC.Show (Show(..))
import Data.Sequence (Seq)

import Protolude

data Fun =
  Fun {
    parameters :: Seq Text,
    isVariadic :: Bool,
    atom :: Atom
  }

data BuiltIn
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
  deriving (Eq, Show)

data Atom
  = Function Fun
  | Bool Bool
  | Error Text
  | Symbol Text
  | Macro Fun
  | MultiArityFn Fun
  | Str Text
  | BuiltIn BuiltIn
  | Number Rational
  | List [Atom]
  | Nil

isSymbol :: Atom -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False
