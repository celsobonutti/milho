module MilhoPrelude (module Data.Text.IO, module Prelude, MonadIO, module Data.Text, ReaderT (..), Reader (..), toS, liftIO, head, show) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (Reader (..), ReaderT (..))
import Data.Text (Text, lines, pack, unlines, unpack, unwords, words)
import Data.Text.IO
import qualified GHC.Show as Show
import Prelude hiding (
    appendFile,
    getContents,
    getLine,
    head,
    interact,
    lines,
    putStr,
    putStrLn,
    readFile,
    show,
    unlines,
    unwords,
    words,
    writeFile,
    writeLine,
 )

toS :: Text -> String
toS = unpack

head :: [a] -> Maybe a
head = \case
    [] -> Nothing
    (x : xs) -> Just x

show :: Show a => a -> Text
show x =
    pack $ Show.show x
