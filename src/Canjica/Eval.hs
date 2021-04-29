module Canjica.Eval where

import Data.Map (Map)
import qualified Data.Map as M
import Pipoquinha.Types.Data (Atom(..))
import Data.Text (Text)
import Canjica.VarTable
import Canjica.Apply

eval :: Atom -> VarTable -> Atom
eval atom table =
  case atom of
    List list -> apply list table
    Symbol _ -> Error "memes"
    n@(Number _) -> n
    f@(Function _) -> f
    m@(Macro _) -> m
    m@(MultiArityFn _) -> m
    b@(Bool _) -> b
    e@(Error _) -> e
    s@(Str _) -> s
    b@(BuiltIn _) -> b
    Nil -> Nil
