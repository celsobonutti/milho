module Pipoquinha.Parser
  ( sExpLine
  , sExpFile
  ) where

import           Control.Monad.Combinators.NonEmpty
                                                ( endBy1 )
import           Control.Monad.Fail             ( fail )
import           Data.List.NonEmpty
import           Data.Text                      ( Text )
import           Data.Void
import qualified Pipoquinha.BuiltIn            as BuiltIn
import           Pipoquinha.BuiltIn
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp
import           Protolude               hiding ( bool
                                                , list
                                                , many
                                                , some
                                                , try
                                                )
import           Text.Megaparsec         hiding ( endBy1 )
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L

type Parser = Parsec Void Text

builtIn :: Parser BuiltIn.T
builtIn =
  string ".__"
    *>  try
          (choice
            [ Add <$ string "add"
            , Mul <$ string "mul"
            , Negate <$ string "negate"
            , Invert <$ string "invert"
            , Eql <$ string "eq"
            , Defn <$ string "defn"
            , Defmacro <$ string "defmacro"
            , Def <$ string "def"
            , Fn <$ string "fn"
            , Let <$ string "let"
            , If <$ string "if"
            , Read <$ string "read"
            , Eval <$ string "eval"
            , Print <$ string "print"
            , Loop <$ string "loop"
            , Do <$ string "do"
            , Not <$ string "not"
            , Cons <$ string "cons"
            , MakeList <$ string "make-list"
            , Car <$ string "car"
            , Cdr <$ string "cdr"
            , Quote <$ string "quote"
            , Gt <$ string "gt"
            , Numerator <$ string "numerator"
            , Set <$ string "set"
            ]
          )
    <?> "built-in"

number :: Parser Rational
number =
  L.signed
      (return ())
      (do
        numerator   <- L.decimal
        denominator <- optional (char '/' >> L.decimal)
        case denominator of
          Nothing -> return (numerator % 1)
          Just 0  -> fail "denominator cannot be 0"
          Just x  -> return (numerator % x)
      )
    <?> "number"

bool :: Parser Bool
bool =
  choice
      [ True <$ string "True"
      , False <$ string "False"
      , True <$ string "Real"
      , False <$ string "Feiki"
      ]
    <?> "boolean"

str :: Parser Text
str =
  toS <$> between (char '\"') (char '\"') (many $ satisfy (/= '"')) <?> "string"

pair :: Parser SExp.Pair
pair = do
  _    <- char '(' *> space
  head <- sExp `endBy1` space1
  _    <- char '.' *> space1
  tail <- sExp
  _    <- char ')' *> space
  return (make head tail) <?> "pair"
 where
  make (x :| []) y = x :.: y
  make (x :| xs) y = x ::: make (fromList xs) y


list :: Parser [SExp.T]
list =
  between (char '(') (char ')') (space *> sExp `sepBy` space1 <* space)
    <?> "list"

quotedSExp :: Parser SExp.T
quotedSExp =
  (do
      _          <- char '\''
      expression <- sExp
      return (Pair (List [BuiltIn Quote, expression]))
    )
    <?> "quote"

invalidSymbolChars :: [Char]
invalidSymbolChars = "[]() \t\r\n\"'"

invalidSymbolStart :: [Char]
invalidSymbolStart = ".1234567890" <> invalidSymbolChars

pSymbol :: Parser Text
pSymbol = do
  firstChar <- satisfy (`notElem` invalidSymbolStart)
  rest      <- many $ satisfy (`notElem` invalidSymbolChars)
  return (toS (firstChar : rest))

sExp :: Parser SExp.T
sExp =
  choice
      [ SExp.Pair Nil <$ string "Nil"
      , Bool <$> bool
      , try (Number <$> number)
      , Symbol <$> pSymbol
      , Str <$> str
      , BuiltIn <$> builtIn
      , try (Pair <$> pair)
      , Pair . List <$> list
      , quotedSExp
      ]
    <?> "atom"

sExpLine :: Parser SExp.T
sExpLine = sExp <* eof

sExpFile :: Parser [SExp.T]
sExpFile = between space space (sExp `sepBy` space1) <* eof
