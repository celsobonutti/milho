module Canjica.Import
  ( safelyReadFile
  , getInformation
  , getNewPath
  , stdPath
  , ImportInformation(..)
  ) where

import           Control.Exception.Base         ( IOException )
import           Pipoquinha.BuiltIn             ( T(Def, Defmacro, Defn) )
import           Pipoquinha.Environment         ( ReaderCapable )
import qualified Pipoquinha.Environment        as Environment
import           Pipoquinha.Error               ( T(..) )
import qualified Pipoquinha.Error              as Error
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp
import           Protolude
import           System.Directory               ( doesFileExist
                                                , makeAbsolute
                                                )
import           System.Environment             ( lookupEnv )
import           System.FilePath.Posix          ( isRelative
                                                , joinPath
                                                , normalise
                                                )
import           System.IO                      ( FilePath )

data ImportKind
  = Path Text
  | Module Text

data ImportInformation = ImportInformation
  { prefix :: Maybe Text
  , kind   :: ImportKind
  }

getInformation :: SExp.T -> Maybe ImportInformation
getInformation (String path) =
  Just $ ImportInformation { kind = Path path, prefix = Nothing }
getInformation (Symbol moduleName) =
  Just $ ImportInformation { kind = Module moduleName, prefix = Nothing }
getInformation (Pair (List [Symbol "prefix-with", Symbol prefix, pathArg])) =
  ImportInformation (Just prefix) . kind <$> getInformation pathArg
getInformation _ = Nothing

getFilePath :: FilePath -> FilePath -> IO (SExp.Result FilePath)
getFilePath currentPath modulePath = do
  absolutePath <- makeAbsolute filePath
  fileExists   <- doesFileExist absolutePath
  if fileExists
    then return . Right $ absolutePath
    else return . Left . FileError $ toS filePath
 where
  filePath =
    if isRelative modulePath then joinPath [currentPath, modulePath] else modulePath

getModuleFile :: FilePath -> FilePath -> IO (SExp.Result FilePath)
getModuleFile currentPath symbolName = do
  stdLibPath <- stdPath

  let relativeFile  = normalise $ joinPath [currentPath, symbolName <> ".milho"]
      globalLibFile = normalise $ joinPath [stdLibPath, symbolName <> ".milho"]

  relativeExists <- doesFileExist relativeFile
  if relativeExists
    then return . Right $ relativeFile
    else do
      globalExists <- doesFileExist globalLibFile
      if globalExists
        then return . Right $ globalLibFile
        else return . Left . FileError $ toS symbolName

getNewPath :: FilePath -> ImportKind -> IO (SExp.Result FilePath)
getNewPath currentPath (Path   path) = getFilePath currentPath (toS path)
getNewPath currentPath (Module name) = getModuleFile currentPath (toS name)

safelyReadFile :: FilePath -> IO (SExp.Result Text)
safelyReadFile path = do
  doesExist <- doesFileExist path
  if doesExist
    then Right <$> readFile (toS path)
    else return . Left . FileError $ toS path

stdPath :: IO FilePath
stdPath = fromMaybe "/opt/milho" <$> lookupEnv "MILHO_STD_PATH"
