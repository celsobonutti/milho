module Canjica.Let where

import Canjica.Function (Arguments)
import Data.Map (Map)
import qualified Data.Map as Map
import MilhoPrelude
import Pipoquinha.Error (T (..))
import Pipoquinha.SExp as SExp

makeTable :: [SExp.T] -> SExp.Result Arguments
makeTable (Symbol s : value : tail) = do
    tailMap <- makeTable tail
    if not (Map.member s tailMap)
        then Right $ Map.insert s value tailMap
        else Left RepeatedParameter
makeTable [] = Right Map.empty
makeTable _ = Left MalformedLet
