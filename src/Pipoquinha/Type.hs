module Pipoquinha.Type where

import           Protolude

data T
  = Function
  | Bool
  | Error
  | Symbol
  | Macro
  | String
  | Number
  | QuotedSymbol
  | ImportPrefixWith
  | Pair deriving (Eq, Ord, Show)
