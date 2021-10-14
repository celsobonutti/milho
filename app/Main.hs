{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Canjica.EvalApply
import           Capability.Reader
import           Capability.State
import           Capability.Error
import           Data.FileEmbed
import           Data.IORef
import           Data.Text (strip)
import qualified Data.Map                      as Map
import           Pipoquinha.Error (T (ParserError))
import qualified Pipoquinha.Parser as Parser
import           Pipoquinha.Environment ( StateCapable
                                        , ReaderCapable
                                        , CatchCapable
                                        )
import qualified Pipoquinha.Environment as Environment
import qualified Pipoquinha.SExp as SExp
import           Pipoquinha.SExp (T (Error)) 
import           Prelude                        ( IO )
import           Protolude                      hiding (catch)
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
      let
        table = Environment.Table { variables = Map.empty
                                  , parent = Nothing
                                  }
      tableRef <- newIORef table
      let environment = Environment.T { table = tableRef }
      putStrLn ("Welcome to the 🌽 REPL!" :: Text)
      mapM_ (\atom -> Environment.runM (eval atom) environment) builtIns
      forever $ Environment.runM run environment

run
  :: (StateCapable SExp.T m, ReaderCapable SExp.T m, CatchCapable m)
  => m ()
run = do
  liftIO $ putStr ("🌽> " :: Text)
  input <- liftIO getLine
  case parse Parser.sExpLine mempty input of
    Left  bundle -> print . Error . ParserError . toS . errorBundlePretty $ bundle
    Right atom   -> do
      atom <- catch @"runtimeError"
              (eval atom)
              (return . Error)
      print atom

parseFile :: ByteString -> Either Text [SExp.T]
parseFile =
  first (toS . errorBundlePretty) . parse Parser.sExpFile mempty . strip . decodeUtf8
  
