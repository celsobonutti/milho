module Canjica.Apply (apply) where 

import Protolude
import Canjica.VarTable
import Pipoquinha.Types.Data (Atom(..), BuiltIn(..))
import Pipoquinha.Types.Atom

apply :: [Atom] -> VarTable -> Atom
apply [] _ = Nil
apply (BuiltIn b : as) vars =
  case b of
    Add -> foldr add (Number 0) as
    _ -> Error "Not implemented yet."
apply _ _ = undefined
