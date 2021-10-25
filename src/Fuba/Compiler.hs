module Fuba.Compiler where

import qualified Binaryen.Expression           as BExpression
import           Binaryen.Index
import qualified Binaryen.Module               as BModule
import qualified Binaryen.Op                   as BOp
import qualified Binaryen.Type                 as BType
import           Data.Word
import           Foreign.C.String
import           Foreign.Ptr
import           Protolude

compile :: IO ()
compile = do
    wasmModule  <- BModule.create
    x           <- BExpression.localGet wasmModule (Index 0) BType.int64
    one         <- BExpression.constInt64 wasmModule 1
    addOne      <- BExpression.binary wasmModule BOp.addInt64 x one
    fName       <- newCString "myfunction"
    addFunction <- BModule.addFunction wasmModule
                                       fName
                                       BType.int64
                                       BType.int64
                                       nullPtr
                                       (Index 0)
                                       addOne
    BModule.addFunctionExport wasmModule fName fName
    BModule.optimize wasmModule
    BModule.print wasmModule
    return ()
