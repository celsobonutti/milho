module Canjica.Import
    ( safelyReadFile
    , prefixDef
    , getInformation
    ) where

import           Control.Exception.Base         ( IOException )
import           Pipoquinha.BuiltIn             ( T(Def, Defmacro, Defn) )
import           Pipoquinha.Environment         ( ReaderCapable )
import           Pipoquinha.Error               ( T(..) )
import qualified Pipoquinha.Error              as Error
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp
import           Protolude
import           System.Directory               ( doesFileExist )
import           System.IO                      ( FilePath )

safelyReadFile :: FilePath -> IO (SExp.Result Text)
safelyReadFile path = do
    doesExist <- doesFileExist path
    if doesExist
        then Right <$> readFile (toS path)
        else return . Left . FileError $ toS path

prefixDef :: Maybe Text -> SExp.T -> SExp.T
prefixDef prefix sexp = case sexp of
    Pair (List (op : Symbol name : rest)) | op `elem` definitionOps ->
        Pair . List $ op : Symbol (fromMaybe "" prefix <> name) : rest
    other -> other

definitionOps :: [SExp.T]
definitionOps =
    [ BuiltIn Def
    , BuiltIn Defn
    , BuiltIn Defmacro
    , Symbol "def"
    , Symbol "defn"
    , Symbol "defmacro"
    ]

getInformation :: SExp.T -> (Maybe Text, Maybe Text)
getInformation (String path) = (Just path, Nothing)
getInformation (Symbol moduleName) =
    (Just $ "/" <> moduleName <> ".milho", Nothing)
getInformation (Pair (List [Symbol "prefix-with", Symbol prefix, pathArg])) =
    (fst (getInformation pathArg), Just prefix)
getInformation _ = (Nothing, Nothing)
