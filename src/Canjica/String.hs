module Canjica.String where

import           Data.Text                      ( length
                                                , splitOn
                                                )
import           Pipoquinha.Environment         ( ThrowCapable )
import qualified Pipoquinha.Error              as Error
import           Pipoquinha.Error               ( ExpectedType(Simple)
                                                , T(TypeMismatch)
                                                )
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp         hiding ( Result )
import qualified Pipoquinha.Type               as Type
import           Protolude               hiding ( length )

type Result = Either Error.T SExp.T

ifString :: SExp.T -> SExp.T -> (a -> SExp.T) -> (Text -> Text -> a) -> Result
ifString (String x) (String y) dataType op = Right . dataType $ op x y
ifString (String _) y _ _ =
    Left $ TypeMismatch { expected = Simple Type.String, found = SExp.toType y }
ifString x _ _ _ =
    Left $ TypeMismatch { expected = Simple Type.String, found = SExp.toType x }

concat :: SExp.T -> SExp.T -> Result
concat x y = ifString x y String (<>)

split :: SExp.T -> SExp.T -> Result
split x y = ifString x y (Pair . List . fmap String) splitText
  where
    splitText :: Text -> Text -> [Text]
    splitText "" y =
        let charList :: [Char]     = toS y
            stringList :: [[Char]] = return <$> charList
        in  toS <$> stringList
    splitText x y = splitOn x y
