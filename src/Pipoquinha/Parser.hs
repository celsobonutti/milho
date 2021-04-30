module Pipoquinha.Parser where

import Control.Monad.Fail (fail)
import Data.Text (Text)
import Data.Void
import Pipoquinha.Types.Data (Atom (..), BuiltIn (..))
import Protolude hiding (many, some, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

test :: Parser Text
test = try $ string "memes"

builtIn :: Parser BuiltIn
builtIn =
  string ".__"
  *> try (choice
    [ Add <$ string "add",
      Mul <$ string "mul",
      Negate <$ string "negate",
      Invert <$ string "invert",
      Eql <$ string "eq",
      Defn <$ string "defn",
      Defmacro <$ string "defmacro",
      Def <$ string "def",
      Fn <$ string "fn",
      Let <$ string "let",
      If <$ string "if",
      Read <$ string "read",
      Eval <$ string "eval",
      Print <$ string "print",
      Loop <$ string "loop",
      Do <$ string "do",
      Not <$ string "not",
      Cons <$ string "cons",
      MakeList <$ string "make-list",
      Car <$ string "car",
      Cdr <$ string "cdr",
      Quote <$ string "quote",
      Gt <$ string "gt"
    ]) <* string "__" <?> "built-in"

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

pAtomFile :: Parser [Atom]
pAtomFile = (between space space $ pAtom `sepBy` space1) <* eof
