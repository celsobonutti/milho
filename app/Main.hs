module Main where

import Prelude (IO)
import Data.Text (Text)
import Canjica.Eval
import Pipoquinha.Parser.Helpers
import Pipoquinha.Types.Atom
import Text.Megaparsec
import qualified Data.Map as M

import Protolude

main :: IO ()
main = do
  input <- getLine
  putStrLn (run input)


run :: Text -> [Char]
run input =
  case parse pAtomLine "" input of
    Left bundle -> errorBundlePretty bundle
    Right atom -> show $ eval atom M.empty
    
