module Pipoquinha.Parser where

import Control.Monad.Fail (fail)
import Data.Text (Text)
import Data.Void
import Pipoquinha.Types.Data (Atom (..), BuiltIn (..))
import Protolude hiding (many, some)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

builtIn :: Parser BuiltIn
builtIn =
  choice
    [ Add <$ string ".__add__",
      Mul <$ string ".__mul__",
      Negate <$ string ".__negate__",
      Invert <$ string ".__invert__",
      Eql <$ string ".__eq__",
      Def <$ string ".__def__",
      Defn <$ string ".__defn__",
      Defmacro <$ string ".__defmacro__",
      Fn <$ string ".__fn__",
      Let <$ string ".__let__",
      If <$ string ".__if__",
      Read <$ string ".__read__",
      Eval <$ string ".__eval__",
      Print <$ string ".__loop__",
      Loop <$ string ".__print__",
      Do <$ string ".__do__",
      Not <$ string ".__not__",
      Cons <$ string ".__cons__",
      MakeList <$ string ".__make-list__",
      Car <$ string ".__car__",
      Cdr <$ string ".__cdr__",
      Quote <$ string ".__quote__",
      Gt <$ string ".__gt__"
    ] <?> "built-in"

pNumber :: Parser Rational
pNumber =
  L.signed
    (return ())
    ( do
        numerator <- L.decimal
        denominator <- optional (char '/' >> L.decimal)
        case denominator of
          Nothing -> return (numerator % 1)
          Just 0 -> fail "denominator cannot be 0"
          Just x -> return (numerator % x)
    ) <?> "number"

pBool :: Parser Bool
pBool =
  choice
    [ True <$ string "True",
      False <$ string "False",
      True <$ string "Real",
      False <$ string "Feiki"
    ] <?> "boolean"

pString :: Parser Text
pString = toS <$> between (char '\"') (char '\"') (many $ satisfy (/= '"')) <?> "string"

pList :: Parser [Atom]
pList = between (char '(') (char ')') (space *> pAtom `sepBy` space1 <* space) <?> "list"

pQuotedAtom :: Parser Atom
pQuotedAtom = (do
  _ <- char '\''
  a <- pAtom
  return (List [BuiltIn Quote, a])) <?> "quote"

invalidSymbolChars :: [Char]
invalidSymbolChars = "[]() \t\r\n\"'"

invalidSymbolStart :: [Char] 
invalidSymbolStart = ".1234567890" <> invalidSymbolChars

pSymbol :: Parser Text 
pSymbol = do
  firstChar <- satisfy (`notElem` invalidSymbolStart)
  rest <- many $ satisfy (`notElem` invalidSymbolChars)
  return (toS (firstChar : rest))

pAtom :: Parser Atom
pAtom =
  choice
    [ Nil <$ string "Nil",
      Bool <$> pBool,
      Symbol <$> pSymbol,
      Number <$> pNumber,
      Str <$> pString,
      BuiltIn <$> builtIn,
      List <$> pList,
      pQuotedAtom
    ] <?> "atom"

pAtomLine :: Parser Atom
pAtomLine = pAtom <* eof

pAtoms :: Parser [Atom]
pAtoms = pAtom `sepBy` space1 <* eof
