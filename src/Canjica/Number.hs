module Canjica.Number where

import           Capability.Error
import           Pipoquinha.Environment         ( ThrowCapable )
import qualified Pipoquinha.Error              as Error
import           Pipoquinha.Error               ( T(TypeMismatch) )
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp                ( T(Number) )
import           Protolude               hiding ( throw )

add :: ThrowCapable m => SExp.T -> SExp.T -> m SExp.T
add (Number x) (Number y) = return $ Number (x + y)
add x          y          = throw @"runtimeError" TypeMismatch

mul :: ThrowCapable m => SExp.T -> SExp.T -> m SExp.T
mul (Number x) (Number y) = return $ Number (x * y)
mul x          y          = throw @"runtimeError" TypeMismatch
