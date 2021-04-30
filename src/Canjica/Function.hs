module Canjica.Function where

import Canjica.VarTable
import qualified Data.Map as Map
import Pipoquinha.Types.Data
import Protolude

create :: VarTable -> [Atom] -> (Atom, VarTable)
create vars [Symbol name, List parameters, body]
  | all isSymbol parameters =
    let extractText :: Atom -> [Text] -> [Text]
        extractText (Symbol name) list = name : list
        extractText _ list = list
        paramNames = foldr extractText [] parameters
     in (Symbol name, Map.insert name (Function $ Fun {atom = body, isVariadic = False, parameters = paramNames}) vars)
  | otherwise = (Error "Every value in the parameter list ", vars)
create vars _ = (Error "Invalid function arguments", vars)

