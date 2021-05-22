module Canjica.Function where

import Capability.State
import Canjica.State
import Pipoquinha.Types.Data
import Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Protolude

-- Function functions
create :: VarTable -> [Atom] -> Either Text Fun
create scope [List parameters, body]
  | all isSymbol parameters =
    let extractText (Symbol name) list = name : list
        extractText _ list = list
        paramNames = foldr extractText [] parameters
     in Right $ Fun {atom = body, isVariadic = False, parameters = Seq.fromList paramNames, scope}
  | otherwise = Left "Every value in the parameter list must be a symbol"
create _ _ = Left "Invalid function arguments"

proceed :: Fun -> [Atom] -> (VarTable, Atom)
proceed Fun {atom, isVariadic, parameters} arguments
  | length arguments == length parameters = (localTable, atom)
  | otherwise =
    (Map.empty, Error "Wrong number of arguments for function")
  where
    localTable = Map.fromList (zip (toList parameters) arguments)
