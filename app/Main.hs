{-# LANGUAGE TemplateHaskell #-}

module Main where

import Canjica.Eval
import Canjica.VarTable
import Data.ByteString (ByteString)
import Data.FileEmbed
import qualified Data.Map as M
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

main =
  case  loadFile $ (strip . decodeUtf8) basicOps of
    Left e ->
      putStrLn e
    Right builtIns -> do
      _ <- runStateT repl builtIns
      return ()

repl :: MyState
repl = forever $ do
  input <- lift getLine
  result <- state (run input)
  putStrLn (show result :: Text)

run :: Text -> VarTable -> (Atom, VarTable)
run input vars =
  case parse pAtomLine mempty input of
    Left bundle -> (Error (toS (errorBundlePretty bundle) :: Text), vars)
    Right atom -> eval vars atom

loadFile :: Text -> Either Text VarTable
loadFile file =
  case parse pAtomFile mempty file of
    Left bundle -> Left (toS (errorBundlePretty bundle) :: Text)
    Right atoms ->
      Right $ foldl eval'' M.empty atoms
