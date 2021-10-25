module Fuba.Compiler where

import           Data.Word
import           Foreign.C.String
import           Foreign.Ptr
import           LLVM.AST                      as LLVM
import           LLVM.AST.Global                ( Global(..) )
import           LLVM.AST.ParameterAttribute
import           Protolude               hiding ( Type
                                                , moduleName
                                                )

i64 :: Type
i64 = IntegerType 64

myfunction :: Global
myfunction = functionDefaults
    { name       = "myfunction"
    , parameters = ( [ Parameter i64 "fst" [ReadOnly]
                     , Parameter i64 "snd" [ReadOnly]
                     ]
                   , False
                   )
    , returnType = i64
    }

myModule = defaultModule { moduleName        = "test-module"
                         , moduleDefinitions = [GlobalDefinition myfunction]
                         }
