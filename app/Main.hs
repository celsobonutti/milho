{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Canjica.EvalApply
import           Capability.Error
import           Capability.Reader
import           Capability.State
import           Data.FileEmbed
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Text                      ( strip )
import           Pipoquinha.Environment         ( CatchCapable
                                                , ReaderCapable
                                                , StateCapable
                                                )
import qualified Pipoquinha.Environment        as Environment
import           Pipoquinha.Error               ( T(ParserError) )
import qualified Pipoquinha.Parser             as Parser
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp                ( T(Error) )
import           Prelude                        ( IO )
import           Protolude               hiding ( catch )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           Text.Megaparsec         hiding ( State )

basicOps :: ByteString
basicOps = foldr ((<>) . snd) "" $(embedDir "std")

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  case parseFile basicOps of
    Left  e        -> putStrLn e
    Right builtIns -> do
      let table = Environment.Table { variables = Map.empty, parent = Nothing }
      tableRef <- newIORef table
      let environment = Environment.T { table = tableRef }
      mapM_ (\atom -> Environment.runM (eval atom) environment) builtIns
      args <- getArgs
      case args of
        ["repl"] -> do
          putStrLn ("Welcome to the 🌽 REPL!" :: Text)
          forever $ Environment.runM run environment
        ["run", path] -> do
          putStrLn ("Not implemented yet" :: Text)


run :: (StateCapable SExp.T m, ReaderCapable SExp.T m, CatchCapable m) => m ()
run = do
  liftIO $ putStr ("🌽> " :: Text)
  input <- liftIO getLine
  case parse Parser.sExpLine mempty input of
    Left bundle ->
      print . Error . ParserError . toS . errorBundlePretty $ bundle
    Right atom -> do
      result <- eval atom
      print result

parseFile :: ByteString -> Either Text [SExp.T]
parseFile =
  first (toS . errorBundlePretty)
    . parse Parser.sExpFile mempty
    . strip
    . decodeUtf8
