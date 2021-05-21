{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Canjica.EvalApply
import Canjica.State
import Data.IORef
import Data.ByteString (ByteString)
import Data.FileEmbed
import qualified Data.Map as Map
import Data.Text (Text, strip)
import Data.Text.Encoding (decodeUtf8)
import Pipoquinha.Parser
import Pipoquinha.Types.Atom
import Pipoquinha.Types.Data
import Protolude
import Text.Megaparsec hiding (State)
import Prelude (IO)

type MyState = StateT VarTable IO [Char]

basicOps :: ByteString
basicOps = $(embedFile "std/basic.milho")

main :: IO ()
main =
  print "memes"

run :: IO ()
run = do
  table <- newIORef Map.empty
  forever $ do
    input <- getLine
    case parse pAtomLine mempty input of
      Left bundle -> print . Error . toS . errorBundlePretty $ bundle
      Right atom -> do
        atom <- runM (eval atom) (Ctx table)
        print atom

