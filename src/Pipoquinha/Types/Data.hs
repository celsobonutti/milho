module Pipoquinha.Types.Data where

import GHC.Show (Show(..))
import Data.Sequence (Seq)

import Protolude

type VarTable = Map Text Atom

data Fun
  = Simple {
    parameters :: Seq Text,
    body :: Atom,
    scope :: VarTable
  }
  | Variadic {
    parameters :: Seq Text,
    body :: Atom,
    scope :: VarTable
  }
  | MultiArity [Fun]
  deriving (Eq, Ord)

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
  deriving (Eq, Show, Ord)

data Atom
  = Function Fun
  | Bool Bool
  | Error Text
  | Symbol Text
  | Macro Fun
  | Str Text
  | BuiltIn BuiltIn
  | Number Rational
  | List [Atom]
  deriving (Eq, Ord)

isSymbol :: Atom -> Bool
isSymbol (Symbol _) = True
isSymbol _ = False
