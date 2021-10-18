module Canjica.Macro where

import           Canjica.Function               ( Arguments )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( dropEnd
                                                , isSuffixOf
                                                )
import           Pipoquinha.SExp         hiding ( T )
import qualified Pipoquinha.SExp               as SExp
import           Protolude

expand :: Arguments -> SExp.T -> SExp.T
expand table symbol@(Symbol variable) =
    fromMaybe symbol (Map.lookup variable table)
expand table element@(Pair (head ::: Symbol variable ::: Nil)) = if isSpread
    then case Map.lookup varName table of
        Just (Pair arguments) -> Pair $ expand table head ::: arguments
        _                     -> element
    else Pair (expand table head ::: expand table (Symbol variable) ::: Nil)
  where
    isSpread = "..." `isSuffixOf` variable
    varName  = dropEnd 3 variable
expand table (Pair (x :.: y)) = Pair (expand table x :.: expand table y)
expand _     body             = body
