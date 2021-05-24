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
  show (Function (Simple parameters _ _)) = "fn#" <> (show . length $ parameters)
  show (Function (Variadic parameters _ _)) = "fn#variadic." <> (show . length $ parameters)
  show (Function (MultiArity fns)) = unwords . map (show . Function) $ fns
  show (Macro (Simple parameters _ _)) = "macro#" <>  (show . length $ parameters)
  show (Macro (Variadic parameters _ _)) = "macro#variadic." <> (show . length $ parameters)
  show (Macro (MultiArity fns)) = unwords . map (show . Function) $ fns
  show (Number n) = showN n
  show (Str s) = toS s
  show (BuiltIn b) = "BuilIn." <> show b
  show (List []) = "Nil"
  show (List a) = showL a

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

mul :: Atom -> Atom -> Atom
mul (Number x) (Number y) = Number (x * y)
mul e@(Error _) _ = e
mul _ e@(Error _) = e
mul _ _ = Error "Cannot multiply non-numeric values"
