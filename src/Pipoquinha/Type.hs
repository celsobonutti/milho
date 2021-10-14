module Pipoquinha.Type where

import           Protolude

data T
  = Function
  | Boolean
  | Error
  | Symbol
  | Macro
  | String
  | BuiltIn
  | Number
  | Pair deriving (Eq, Ord, Show)
