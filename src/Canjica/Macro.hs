module Canjica.Macro where

import           Canjica.Function               ( Arguments )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Pipoquinha.SExp         hiding ( T )
import qualified Pipoquinha.SExp               as SExp
import           Protolude

expand :: Arguments -> SExp.T -> SExp.T
expand table symbol@(Symbol s) = fromMaybe symbol (Map.lookup s table)
expand table element@(Pair (head ::: Symbol "+rest..." ::: Nil)) =
    case Map.lookup "+rest" table of
        Just (Pair arguments) -> Pair $ head ::: arguments
        _                     -> element
expand table (Pair (x :.: y)) = Pair (expand table x :.: expand table y)
expand _     body             = body
