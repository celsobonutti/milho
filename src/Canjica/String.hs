module Canjica.String where

import Data.Text (
    length,
    pack,
    splitOn,
    unpack,
 )
import MilhoPrelude hiding (length)
import Pipoquinha.Environment (ThrowCapable)
import Pipoquinha.Error (
    ExpectedType (Simple),
    T (TypeMismatch),
 )
import qualified Pipoquinha.Error as Error
import Pipoquinha.SExp hiding (Result)
import qualified Pipoquinha.SExp as SExp
import qualified Pipoquinha.Type as Type

type Result = Either Error.T SExp.T

ifString :: SExp.T -> SExp.T -> (a -> SExp.T) -> (Text -> Text -> a) -> Result
ifString (String x) (String y) dataType op = Right . dataType $ op x y
ifString (String _) y _ _ =
    Left $ TypeMismatch{expected = Simple Type.String, found = SExp.toType y}
ifString x _ _ _ =
    Left $ TypeMismatch{expected = Simple Type.String, found = SExp.toType x}

concat :: SExp.T -> SExp.T -> Result
concat x y = ifString x y String (<>)

split :: SExp.T -> SExp.T -> Result
split x y = ifString x y (Pair . List . fmap String) splitText
  where
    splitText :: Text -> Text -> [Text]
    splitText "" y =
        let charList :: [Char] = unpack y
            stringList :: [[Char]] = return <$> charList
         in pack <$> stringList
    splitText x y = splitOn x y
