module Pipoquinha.VarTable where

import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import MilhoPrelude
import qualified Pipoquinha.SExp as SExp

type VarRef = IORef SExp.T

data Table = Table
    { variables :: Map Text VarRef
    , parent :: Maybe (IORef Table)
    }

type TableRef = IORef Table
