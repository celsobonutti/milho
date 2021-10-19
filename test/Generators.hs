module Generators where

import           GHC.Show                       ( Show(..) )
import           Protolude               hiding ( show )
import           Test.QuickCheck

data Arithmetic
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq)

instance Show Arithmetic where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Arbitrary Arithmetic where
  arbitrary = elements [Add, Sub, Mul, Div]

data BoolOp
  = And
  | Or
  | Not
  deriving (Eq)

instance Show BoolOp where
  show And = "and"
  show Or  = "or"
  show Not = "not"

instance Arbitrary BoolOp where
  arbitrary = elements [And, Or, Not]
