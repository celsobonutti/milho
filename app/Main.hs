module Main where

import Canjica.EvalApply
import qualified Canjica.Std as Std
import Data.Functor ((<&>))
import MilhoPrelude
import qualified Pipoquinha.Environment as Environment
import qualified Pipoquinha.Parser as Parser
import qualified Pipoquinha.SExp as SExp
import System.Directory (getCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath.Posix (addTrailingPathSeparator)
import System.IO (
    BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
 )
import Text.Megaparsec ()

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    currentDirectory <- getCurrentDirectory <&> addTrailingPathSeparator
    environment <- Std.environment currentDirectory
    getArgs >>= \case
        [path] -> do
            content <- liftIO $ readFile path
            case Parser.parseFile content of
                Left e -> putStrLn e
                Right instructions -> mapM_ (runExpression environment) instructions
        _ -> putStrLn ("Invalid option." :: Text)

runExpression :: Environment.T SExp.T -> SExp.T -> IO SExp.T
runExpression environment expression =
    Environment.runM (eval expression) environment
