module Pipoquinha.Types.Atom where

import Protolude hiding (show, unwords)

import Prelude(Show(..), ($)) 
import Data.List (unwords)
import Data.Monoid ((<>))

import Pipoquinha.Types.Data (Atom(..), Fun(..))

instance Show Atom where
  show (Bool b) = show b
  show (Symbol t) = toS $ "#" <> t
  show (Error e) = toS $ "Error: " <> e
  show (Function f) = "fn#" <>  (show . length $ parameters f)
  show (Macro m) = "macro#" <> (show . length $ parameters m)
  show (MultiArityFn _) = "multi-arity-function"
  show (Number n) = showN n
  show (Str s) = toS s
  show (BuiltIn b) = "BuilIn." <> show b
  show (List a) = showL a
  show Nil = "Nil"

showL :: [Atom] -> [Char]
showL atoms = '(' : (unwords . map show $ atoms) ++ ")"

showN :: Rational -> [Char]
showN n
  | denominator n == 1 = show $ numerator n
  | otherwise = show (numerator n) ++ "/" ++ show (denominator n)

add :: Atom -> Atom -> Atom
add (Number x) (Number y) = Number (x + y)
add e@(Error _) _ = e
add _ e@(Error _) = e
add _ _ = Error "Cannot add non-numeric values"
