module Pipoquinha.Error where

import           GHC.Show                       ( Show(..) )
import           Protolude               hiding ( show )

data T
  = UndefinedVariable Text
  | TypeMismatch
    deriving Eq

instance Exception T

instance Show T where
    show (UndefinedVariable name) = "Undefined variable: " <> toS name
    show TypeMismatch             = "Type mismatch: expecting {}, found {}"
