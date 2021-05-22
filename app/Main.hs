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
import Capability.State

type MyState = StateT VarTable IO [Char]

basicOps :: ByteString
basicOps = $(embedFile "std/basic.milho")

main :: IO ()
main = do
  case (strip . decodeUtf8) basicOps of
    Left e ->
      putStrLn e
    Right buildInts -> do
      table <- newIORef Map.empty
      mapM_ (\atom -> runM (eval atom) (Ctx table)) []
      forever $ runM run (Ctx table)

run :: (HasState "table" VarTable m, MonadIO m) => m ()
run = do
    input <- liftIO getLine
    case parse pAtomLine mempty input of
      Left bundle -> print . Error . toS . errorBundlePretty $ bundle
      Right atom -> do
        atom <- eval atom
        print atom

