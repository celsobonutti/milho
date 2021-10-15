module Pipoquinha.SExp where

import           GHC.Show                       ( Show(..) )
import qualified Pipoquinha.BuiltIn            as BuiltIn
import qualified Pipoquinha.Environment        as Environment
import qualified Pipoquinha.Error              as Error
import           Pipoquinha.Function     hiding ( T )
import qualified Pipoquinha.Function           as Function
import qualified Pipoquinha.Type               as Type
import           Protolude               hiding ( show )

data Pair
  = Nil
  | P T T
  deriving (Eq, Ord)

infixr 9 :::

pattern x ::: xs <- P x (Pair xs) where
  x ::: xs = P x (Pair xs)

pattern x :.: y <- P x y where
  x :.: y = P x y

pattern List x <- (pairToList -> Just x) where
  List x = pairFromList x

pairFromList :: [T] -> Pair
pairFromList = foldr (:::) Nil

pairToList :: Pair -> Maybe [T]
pairToList Nil        = Just []
pairToList (x ::: xs) = (x :) <$> pairToList xs
pairToList (x :.: xs) = Nothing

{-# COMPLETE (:.:), (:::), Nil #-}

{-# COMPLETE (:.:), List, Nil #-}

instance Show Pair where
  show Nil         = "Nil"
  show (x ::: Nil) = show x
  show (x ::: xs ) = show x ++ " " ++ show xs
  show (x :.: y  ) = show x ++ " . " ++ show y

type Environment = Environment.TableRef T

type Function = Function.T T

type Result = Either Error.T

data T
  = Function Function
  | Bool Bool
  | Error Error.T
  | Symbol Text
  | Macro Function
  | String Text
  | BuiltIn BuiltIn.T
  | Number Rational
  | Pair Pair
  deriving (Eq, Ord)

instance Show T where
  show (Bool     b) = show b
  show (Symbol   t) = toS $ "'" <> t
  show (Error    e) = toS $ "Error: " <> show e
  show (Function f) = "fn#" <> show f
  show (Macro    m) = "macro#" <> show m
  show (String   s) = "\"" <> toS s <> "\""
  show (BuiltIn  b) = "BuilIn." <> show b
  show (Pair     p) = "(" ++ show p ++ ")"
  show (Number n)
    | denominator n == 1 = show $ numerator n
    | otherwise          = show (numerator n) ++ "/" ++ show (denominator n)

isSymbol :: T -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False

toType :: T -> Type.T
toType (Function _) = Type.Function
toType (Bool     _) = Type.Boolean
toType (Error    _) = Type.Error
toType (Symbol   _) = Type.Symbol
toType (Macro    _) = Type.Macro
toType (String   _) = Type.String
toType (BuiltIn  _) = Type.BuiltIn
toType (Number   _) = Type.Number
toType (Pair     _) = Type.Pair
