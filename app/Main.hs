{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Canjica.EvalApply
import           Capability.Reader
import           Capability.State
import           Data.ByteString                ( ByteString )
import           Data.ByteString.Char8          ( split )
import           Data.FileEmbed
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Text                      ( Text
                                                , strip
                                                )
import           Data.Text.Encoding             ( decodeUtf8 )
import qualified Pipoquinha.Parser as Parser
import           Prelude                        ( IO )
import           Protolude
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           Text.Megaparsec         hiding ( State )

type MyState = StateT VarTable IO [Char]

basicOps :: ByteString
basicOps = foldr ((<>) . snd) "" $(embedDir "std")

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  case parseFile basicOps of
    Left  e        -> putStrLn e
    Right builtIns -> do
      table <- newIORef Map.empty
      let
        environment = Global table
      mapM_ (\atom -> runM (eval atom) (Ctx environment)) builtIns
      forever $ runM run (Ctx environment)

run
  :: (HasReader "environment" Environment m, MonadIO m)
  => m ()
run = do
  input <- liftIO getLine
  case parse Parser.sExpLine mempty input of
    Left  bundle -> print . Error . toS . errorBundlePretty $ bundle
    Right atom   -> do
      atom <- eval atom
      print atom

parseFile :: ByteString -> Either [Char] [Atom]
parseFile =
  first errorBundlePretty . parse Parser.sExpFile mempty . strip . decodeUtf8
