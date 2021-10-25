{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Canjica.EvalApply
import qualified Canjica.Std                   as Std
import           Capability.Error
import           Capability.Reader
import           Capability.State
import           Data.FileEmbed
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Text                      ( strip )
import           Fuba.Compiler
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
import           System.Directory               ( getCurrentDirectory )
import           System.FilePath.Posix          ( addTrailingPathSeparator )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , stdout
                                                )
import           Text.Megaparsec         hiding ( State )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  currentDirectory <- getCurrentDirectory <&> addTrailingPathSeparator
  environment      <- Std.environment currentDirectory
  getArgs >>= \case
    []     -> compile
    [path] -> do
      content <- liftIO $ readFile path
      case Parser.parseFile content of
        Left  e            -> putStrLn e
        Right instructions -> mapM_ (runExpression environment) instructions
    _ -> putStrLn ("Invalid option." :: Text)

runExpression environment expression =
  Environment.runM (eval expression) environment
