{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Pipoquinha.Types.Data where

import           Capability.Reader       hiding ( (:.:) )
import           Capability.Sink         hiding ( (:.:) )
import           Capability.Source       hiding ( (:.:) )
import           Capability.State        hiding ( (:.:) )
import           Data.IORef
import           Data.List                      ( unwords )
import qualified Data.Map                      as Map
import           Data.Sequence                  ( Seq )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( Show(..) )
import           Protolude               hiding ( MonadReader
                                                , ask
                                                , get
                                                , put
                                                , show
                                                , unwords
                                                )

data Pair
  = Nil
  | P Atom Atom
  deriving (Eq, Ord)

infixr 9 :::

pattern x ::: xs <- P x (Pair xs) where
  x ::: xs = P x (Pair xs)

pattern x :.: y <- P x y where
  x :.: y = P x y

pattern List x <- (pairToList -> Just x) where
  List x = pairFromList x

pairFromList :: [Atom] -> Pair
pairFromList = foldr (:::) Nil

pairToList :: Pair -> Maybe [Atom]
pairToList Nil        = Just []
pairToList (x ::: xs) = (x :) <$> pairToList xs
pairToList (x :.: xs) = Nothing

{-# COMPLETE (:.:), (:::), Nil #-}

{-# COMPLETE (:.:), List, Nil #-}

instance Show Pair where
  show Nil         = "Nil"
  show (x ::: Nil) = show x
  show (x ::: xs ) = show x ++ " " ++ show xs
  show (x :.: y  ) = show x ++ " . " ++ show y

type VarTable = Map Text Atom

data SimpleFunction = SF
  { parameters  :: Seq Text
  , body        :: Atom
  , environment :: Environment
  }
  deriving Eq

data VariadicFunction = VF
  { parameters  :: Seq Text
  , body        :: Atom
  , environment :: Environment
  }
  deriving Eq

data MultiArityFunction = MAF
  { bodies   :: Map Int SimpleFunction
  , variadic :: Maybe VariadicFunction
  }
  deriving Eq

instance Show MultiArityFunction where
  show _ = "multi-arity-function"

data Fun
  = Simple SimpleFunction
  | Variadic VariadicFunction
  | MultiArity MultiArityFunction
  deriving (Eq)

instance Ord Fun where
  compare _ _ = EQ

data BuiltIn
  = Add
  | Mul
  | Negate
  | Invert
  | Eql
  | Def
  | Defn
  | Defmacro
  | Fn
  | Let
  | If
  | Read
  | Eval
  | Print
  | Loop
  | Do
  | Not
  | Cons
  | MakeList
  | Car
  | Cdr
  | Quote
  | Gt
  | Numerator
  | Set
  deriving (Eq, Show, Ord)

data Atom
  = Function Fun
  | Bool Bool
  | Error Text
  | Symbol Text
  | Macro Fun
  | Str Text
  | BuiltIn BuiltIn
  | Number Rational
  | Pair Pair
  deriving (Eq, Ord)

isSymbol :: Atom -> Bool
isSymbol (Symbol _) = True
isSymbol _          = False

instance Show Atom where
  show (Bool   b) = show b
  show (Symbol t) = toS $ "#" <> t
  show (Error  e) = toS $ "Error: " <> e
  show (Function (Simple (SF parameters _ _))) =
    "fn#" <> (show . length $ parameters)
  show (Function (Variadic (VF parameters _ _))) =
    "fn#variadic." <> (show . length $ parameters)
  show (Function (MultiArity fns)) = show fns
  show (Macro (Simple (SF parameters _ _))) =
    "macro#" <> (show . length $ parameters)
  show (Macro (Variadic (VF parameters _ _))) =
    "macro#variadic." <> (show . length $ parameters)
  show (Macro   (MultiArity fns)) = show fns
  show (Number  n               ) = showN n
  show (Str     s               ) = toS s
  show (BuiltIn b               ) = "BuilIn." <> show b
  show (Pair    p               ) = "(" ++ show p ++ ")"

showN :: Rational -> [Char]
showN n | denominator n == 1 = show $ numerator n
        | otherwise          = show (numerator n) ++ "/" ++ show (denominator n)

add :: Atom -> Atom -> Atom
add (  Number x) (Number y)  = Number (x + y)
add e@(Error  _) _           = e
add _            e@(Error _) = e
add _            _           = Error "Cannot add non-numeric values"

mul :: Atom -> Atom -> Atom
mul (  Number x) (Number y)  = Number (x * y)
mul e@(Error  _) _           = e
mul _            e@(Error _) = e
mul _            _           = Error "Cannot multiply non-numeric values"

data Environment
  = Global (IORef VarTable)
  | Local { table :: IORef VarTable
          , parent :: Environment
          } deriving (Eq)

getTable :: Environment -> IORef VarTable
getTable (Global table) = table
getTable Local { table } = table

setValue key value (getTable -> tableRef) = do
  table <- readIORef tableRef
  writeIORef tableRef (Map.insert key value table)

getVariableEnvironment var e@(Global table) = do
  table <- readIORef table
  return (e <$ Map.lookup var table)
getVariableEnvironment var e@Local {table, parent} = do
  table <- readIORef table
  case Map.lookup var table of
    Nothing -> liftIO (getVariableEnvironment var parent)
    Just _ -> return . Just $ e

getValue var (Global table) = do
  table <- readIORef table
  return (Map.lookup var table)
getValue var Local {table, parent} = do
  table <- readIORef table
  case Map.lookup var table of
    Nothing -> liftIO (getValue var parent)
    Just value -> return (Just value)


newtype Ctx = Ctx
  { environment :: Environment
  }
  deriving (Generic)

newtype M a = M {runM :: Ctx -> IO a}
  deriving (Functor, Applicative, Monad, MonadIO) via ReaderT Ctx IO
  deriving
    (HasSource "environment" Environment, HasReader "environment" Environment)
    via Field "environment" "ctx" (MonadReader (ReaderT Ctx IO))
