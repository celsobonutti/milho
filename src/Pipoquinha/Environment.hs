module Pipoquinha.Environment where

import           Capability.Constraints
import           Capability.Error
import           Capability.Reader
import           Capability.Sink
import           Capability.Source
import           Capability.State
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Pipoquinha.Error              as Error
import           Protolude               hiding ( All
                                                , MonadReader
                                                , ask
                                                , asks
                                                , get
                                                , gets
                                                , modify'
                                                , put
                                                )

data Table sexp = Table
  { variables :: Map Text (IORef sexp)
  , parent    :: Maybe (IORef (Table sexp))
  }
  deriving Eq

insert :: Text -> IORef sexp -> Table sexp -> Table sexp
insert key value table =
  table { variables = Map.insert key value (variables table) }

type TableRef sexp = IORef (Table sexp)

newtype T sexp = T
  { table :: TableRef sexp
  }
  deriving Generic

newtype M sexp a = M {runM :: T sexp -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT (T sexp) IO

  deriving
    ( HasSource "table" (TableRef sexp)
    , HasReader "table" (TableRef sexp)
    ) via Field "table" "environment" (MonadReader (ReaderT (T sexp) IO))

  deriving
    ( HasThrow "runtimeError" Error.T
    , HasCatch "runtimeError" Error.T
    ) via MonadUnliftIO Error.T (ReaderT (T sexp) IO)

type ReaderCapable sexp m = (HasReader "table" (TableRef sexp) m, MonadIO m)

type ThrowCapable m = HasThrow "runtimeError" Error.T m

insertValue :: ReaderCapable sexp m => Text -> sexp -> m ()
insertValue key value = do
  valueRef <- liftIO $ newIORef value
  table    <- ask @"table"
  liftIO $ modifyIORef table (insert key valueRef)

getValue :: (ThrowCapable m, MonadIO m) => Text -> Table sexp -> m (IORef sexp)
getValue key Table { variables, parent } = case Map.lookup key variables of
  Just value -> return value
  Nothing    -> case fmap readIORef parent of
    Nothing          -> throw @"runtimeError" (Error.UndefinedVariable key)
    Just parentTable -> liftIO parentTable >>= getValue key

setValue :: (ThrowCapable m, MonadIO m) => Text -> sexp -> Table sexp -> m ()
setValue key newValue table = do
  value <- getValue key table
  liftIO $ writeIORef value newValue
