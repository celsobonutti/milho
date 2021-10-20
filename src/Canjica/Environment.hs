module Canjica.Environment where

import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           GHC.Show                       ( Show(..) )
import qualified Pipoquinha.BuiltIn            as BuiltIn
import qualified Pipoquinha.Environment        as Environment
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp                ( T(BuiltIn) )
import           Protolude               hiding ( show )
import           System.FilePath.Posix

basicEnvironment :: FilePath -> IO (Environment.T SExp.T)
basicEnvironment executionPath = do
    builtIns <- mapM newIORef builtInMap
    tableRef <- newIORef
        $ Environment.Table { variables = builtIns, parent = Nothing }
    return $ Environment.T { table = tableRef, executionPath }
  where
    toPair :: BuiltIn.T -> (Text, SExp.T)
    toPair builtIn = (toS . show $ builtIn, BuiltIn builtIn)
    builtInMap :: Map Text SExp.T
    builtInMap = Map.fromList $ fmap toPair [minBound ..]
