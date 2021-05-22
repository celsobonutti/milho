{-# LANGUAGE TemplateHaskell #-}

module Main where

import Canjica.EvalApply
import Canjica.State
import Data.IORef
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (split)
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
import Capability.Reader
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

type MyState = StateT VarTable IO [Char]

basicOps :: ByteString
basicOps = $(embedFile "std/basic.milho")

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  case parseFile basicOps of
    Left e ->
      putStrLn e
    Right builtIns -> do
      table <- newIORef Map.empty
      mapM_ (\atom -> runM (eval atom) (Ctx table Map.empty)) builtIns
      forever $ runM run (Ctx table Map.empty)

run :: (HasState "table" VarTable m, HasReader "localScope" VarTable m, MonadIO m) => m ()
run = do
    input <- liftIO getLine
    case parse pAtomLine mempty input of
      Left bundle -> print . Error . toS . errorBundlePretty $ bundle
      Right atom -> do
        atom <- eval atom
        print atom

parseFile :: ByteString -> Either [Char] [Atom]
parseFile = first errorBundlePretty . parse pAtomFile mempty . strip  . decodeUtf8
