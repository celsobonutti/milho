module Canjica.Number
   ( add
   , mul
   , gt
   , lt
   ) where

import           Capability.Error
import           Pipoquinha.Environment         ( ThrowCapable )
import qualified Pipoquinha.Error              as Error
import           Pipoquinha.Error               ( T(TypeMismatch) )
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp                ( T(Bool, Number) )
import qualified Pipoquinha.Type               as Type
import           Protolude               hiding ( throw )

type Result = Either Error.T SExp.T

ifNumber
   :: SExp.T -> SExp.T -> (a -> SExp.T) -> (Rational -> Rational -> a) -> Result
ifNumber (Number x) (Number y) dataType op = Right . dataType $ op x y
ifNumber (Number _) y _ _ =
   Left $ TypeMismatch { expected = Type.Number, found = SExp.toType y }
ifNumber x _ _ _ =
   Left $ TypeMismatch { expected = Type.Number, found = SExp.toType x }

add :: SExp.T -> SExp.T -> Result
add x y = ifNumber x y Number (+)

mul :: SExp.T -> SExp.T -> Result
mul x y = ifNumber x y Number (*)

gt :: SExp.T -> SExp.T -> Result
gt x y = ifNumber x y Bool (>)

lt :: SExp.T -> SExp.T -> Result
lt x y = ifNumber x y Bool (<)
