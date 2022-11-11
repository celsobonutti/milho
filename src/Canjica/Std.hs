module Canjica.Std (
    environment,
) where

import Canjica.EvalApply (eval)
import Data.IORef
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Show (Show (..))
import MilhoPrelude hiding (show)
import qualified Pipoquinha.BuiltIn as BuiltIn
import qualified Pipoquinha.Environment as Environment
import qualified Pipoquinha.ImportStack as ImportStack
import Pipoquinha.Parser (parseExpression)
import Pipoquinha.SExp (T (BuiltIn))
import qualified Pipoquinha.SExp as SExp
import System.FilePath.Posix

environment :: FilePath -> IO (Environment.T SExp.T)
environment executionPath = do
    environment <-
        Environment.make
            builtInMap
            (ImportStack.singleton executionPath)
            Nothing
    Environment.runM (eval importExpression) environment
    return environment
  where
    toPair :: BuiltIn.T -> (Text, SExp.T)
    toPair builtIn = (pack . show $ builtIn, BuiltIn builtIn)
    builtInMap :: Map Text SExp.T
    builtInMap = Map.fromList $ fmap toPair [minBound ..]
    importExpression :: SExp.T
    importExpression = parseExpression "(import std)"
