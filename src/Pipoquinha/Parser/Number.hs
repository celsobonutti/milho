{-# LANGUAGE OverloadedStrings #-}
module Pipoquinha.Parser.Number where

import Protolude hiding (try)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Fail (fail)
import Data.Text (Text)

type Parser = Parsec Void Text

number :: Parser Rational
number =
  L.signed (return ()) (
        do
          numerator <- L.decimal
          denominator <- optional (char '/' >> L.decimal)
          case denominator of
            Nothing -> return (numerator % 1)
            Just 0 -> fail "denominator cannot be 0"
            Just x -> return (numerator % x)
        ) 
