module Canjica.Boolean where

import Capability.Error
import MilhoPrelude hiding (throw)
import Pipoquinha.Environment (ThrowCapable)
import Pipoquinha.Error (
    ExpectedType (Simple),
    T (TypeMismatch),
 )
import qualified Pipoquinha.Error as Error
import Pipoquinha.SExp (T (Bool, Number))
import qualified Pipoquinha.SExp as SExp
import qualified Pipoquinha.Type as Type

type Result = Either Error.T SExp.T

ifBoolean :: SExp.T -> SExp.T -> (a -> SExp.T) -> (Bool -> Bool -> a) -> Result
ifBoolean (Bool x) (Bool y) dataType op = Right . dataType $ op x y
ifBoolean (Bool _) y _ _ =
    Left $ TypeMismatch{expected = Simple Type.Bool, found = SExp.toType y}
ifBoolean x _ _ _ =
    Left $ TypeMismatch{expected = Simple Type.Bool, found = SExp.toType x}

and :: SExp.T -> SExp.T -> Result
and x y = ifBoolean x y Bool (&&)

or :: SExp.T -> SExp.T -> Result
or x y = ifBoolean x y Bool (||)
