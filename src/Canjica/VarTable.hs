module Canjica.VarTable where

import Data.Text (Text)
import Data.Map (Map)
import Pipoquinha.Types.Data (Atom)

type VarTable = Map Text Atom
