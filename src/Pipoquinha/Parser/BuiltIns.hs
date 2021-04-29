module Pipoquinha.Parser.BuiltIns where

import Prelude
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import Pipoquinha.Types.Data (BuiltIn(..))
import Pipoquinha.Parser.Helpers (Parser)

