{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Pipoquinha.ImportStack (
    T,
    singleton,
    current,
    push,
) where

import Data.List.NonEmpty hiding (singleton)
import MilhoPrelude
import System.FilePath.Posix (FilePath)

newtype Stack a = Wrap {unwrap :: NonEmpty a} deriving (Foldable)

type T = Stack FilePath

singleton :: FilePath -> T
singleton path = Wrap (path :| [])

current :: T -> FilePath
current Wrap{unwrap = head :| _} = head

push :: FilePath -> T -> T
push new = Wrap . (new :|) . toList . (.unwrap)
