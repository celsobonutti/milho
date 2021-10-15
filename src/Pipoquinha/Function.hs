module Pipoquinha.Function where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           GHC.Show                       ( Show(..) )
import qualified Pipoquinha.Environment        as Environment
import           Protolude               hiding ( show )

data Simple sexp = SF
  { parameters  :: Seq Text
  , name        :: Maybe Text
  , body        :: sexp
  , environment :: Environment.TableRef sexp
  }
  deriving Eq

data Variadic sexp = VF
  { parameters  :: Seq Text
  , name        :: Maybe Text
  , body        :: sexp
  , environment :: Environment.TableRef sexp
  }
  deriving Eq

data MultiArity sexp = MAF
  { bodies   :: Map Int (Simple sexp)
  , variadic :: Maybe (Variadic sexp)
  , name     :: Maybe Text
  }
  deriving Eq

data T sexp
  = Simple (Simple sexp)
  | Variadic (Variadic sexp)
  | MultiArity (MultiArity sexp)
  deriving (Eq)

instance Eq a => Ord (T a) where
  compare _ _ = EQ

instance Show (T a) where
  show (Simple   SF { parameters }) = show . length $ parameters
  show (Variadic VF { parameters }) = (show . length $ parameters) <> "+"
  show (MultiArity MAF { bodies, variadic }) =
    "{" ++ showBodies bodies ++ maybe "" showVariadic variadic ++ "}"
   where
    showBodies :: Map Int (Simple a) -> [Char]
    showBodies sexp =
      intercalate ", "
        . fmap (\SF { parameters } -> show . length $ parameters)
        . toList
        $ bodies
    showVariadic :: Variadic a -> [Char]
    showVariadic function = ", " <> show (Variadic function)
