module Canjica.Import
    ( safelyReadFile
    , renameDef
    ) where

import           Control.Exception.Base         ( IOException )
import           Pipoquinha.BuiltIn             ( T(Def, Defmacro, Defn) )
import qualified Pipoquinha.Environment        as Environment
import           Pipoquinha.Error               ( T(..) )
import qualified Pipoquinha.Error              as Error
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp
import           Protolude

safelyReadFile :: Text -> IO (SExp.Result Text)
safelyReadFile path =
    catch (Right <$> readFile (toS path)) (catchAsFileError path)

catchAsFileError :: Text -> IOException -> IO (SExp.Result Text)
catchAsFileError path _ = return . Left $ FileError path

renameDef :: Text -> SExp.T -> SExp.T
renameDef scope sexp = case sexp of
    Pair (List (op : Symbol name : rest)) | op `elem` definitionOps ->
        Pair . List $ op : Symbol (scope <> "::" <> name) : rest
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
