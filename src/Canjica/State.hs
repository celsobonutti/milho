module Canjica.State where

import Capability.Reader
import Capability.Sink
import Capability.Source
import Capability.State
import Data.IORef
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Pipoquinha.Types.Atom
import Pipoquinha.Types.Data
import Protolude hiding (MonadReader, ask, get, put)

type VarTable = Map Text Atom

data Ctx = Ctx
  { table :: IORef VarTable,
    localScope :: VarTable
  }
  deriving (Generic)

newtype M a = M {runM :: Ctx -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT Ctx IO
  deriving
    (HasSource "localScope" VarTable, HasReader "localScope" VarTable)
    via Field "localScope" "ctx" (MonadReader (ReaderT Ctx IO))
   deriving
    (HasSource "table" VarTable, HasSink "table" VarTable, HasState "table" VarTable)
    via ReaderIORef (Field "table" "ctx" (MonadReader (ReaderT Ctx IO)))
