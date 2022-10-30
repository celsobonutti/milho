module Pipoquinha.VarTable where

import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Pipoquinha.SExp               as SExp
import           MilhoPrelude

type VarRef = IORef SExp.T

data Table = Table
    { variables :: Map Text VarRef
    , parent    :: Maybe (IORef Table)
    }

type TableRef = IORef Table
