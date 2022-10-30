module Canjica.Macro where

import           Canjica.Function               ( Arguments )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Text                      ( dropEnd
                                                , isSuffixOf
                                                )
import           Pipoquinha.Parser              ( parseExpression )
import           Pipoquinha.SExp         hiding ( T )
import Data.Maybe (fromMaybe)
import Data.Text (isSuffixOf)
import qualified Pipoquinha.SExp               as SExp
import           MilhoPrelude hiding (isSuffixOf)

expand :: Arguments -> SExp.T -> SExp.T
expand table symbol@(Symbol variable) =
    fromMaybe symbol (Map.lookup variable table)
expand table (Pair (List elements)) =
    Pair (List (elements >>= spreadSymbols table))
expand table (Pair (x :.: y)) = Pair (expand table x :.: expand table y)
expand _     body             = body

spreadSymbols :: Arguments -> SExp.T -> [SExp.T]
spreadSymbols table sexp = case sexp of
    Symbol name | isSpread name -> case Map.lookup (varName name) table of
        Just (Pair (List arguments)) -> arguments
        _                            -> [Symbol name]
    element -> [expand table element]
  where
    isSpread name = "..." `isSuffixOf` name
    varName = dropEnd 3
