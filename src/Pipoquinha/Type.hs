module Pipoquinha.Type where

import           GHC.Show                       ( Show(..) )
import           MilhoPrelude               hiding ( show )

data T
  = Function
  | Bool
  | Error
  | Symbol
  | Macro
  | String
  | Number
  | Pair
  | QuotedSymbol
  | ImportPrefixWith deriving (Eq, Ord)

instance Show T where
  show Function         = "function"
  show Error            = "error"
  show Symbol           = "symbol"
  show Bool             = "bool"
  show Macro            = "macro"
  show String           = "string"
  show Number           = "number"
  show Pair             = "pair"
  show QuotedSymbol     = "quoted-symbol"
  show ImportPrefixWith = "import-prefix"
