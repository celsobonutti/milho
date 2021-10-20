module Canjica.Std
    ( environment
    ) where

import           Canjica.EvalApply              ( eval )
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           GHC.Show                       ( Show(..) )
import qualified Pipoquinha.BuiltIn            as BuiltIn
import qualified Pipoquinha.Environment        as Environment
import qualified Pipoquinha.ImportStack        as ImportStack
import           Pipoquinha.Parser              ( parseExpression )
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp                ( T(BuiltIn) )
import           Protolude               hiding ( show )
import           System.FilePath.Posix

environment :: FilePath -> IO (Environment.T SExp.T)
environment executionPath = do
    environment <- Environment.make builtInMap
                                    (ImportStack.singleton executionPath)
                                    Nothing
    Environment.runM (eval importExpression) environment
    return environment
  where
    toPair :: BuiltIn.T -> (Text, SExp.T)
    toPair builtIn = (toS . show $ builtIn, BuiltIn builtIn)
    builtInMap :: Map Text SExp.T
    builtInMap = Map.fromList $ fmap toPair [minBound ..]
    importExpression :: SExp.T
    importExpression = parseExpression "(import std)"
