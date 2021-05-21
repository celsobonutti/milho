{-# LANGUAGE TupleSections #-}

module Canjica.Internal where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (pack)
import Pipoquinha.Types.Atom
import Pipoquinha.Types.Data
import Protolude
import qualified Data.Sequence as Seq

-- Apply
-- apply :: [Atom] -> VarTable -> (Atom, VarTable)

-- apply [] vars = (Nil, vars)

-- apply [BuiltIn Def, Symbol name, atom] map =
--   (Symbol name, Map.insert name (eval' map atom) map)

-- apply (BuiltIn Defn : as) vars = create vars as

-- apply (BuiltIn Fn : as) vars = lambda vars as

-- apply (BuiltIn Add : as) vars = (foldr (add . eval' vars) (Number 0) as, vars)

-- apply (BuiltIn Mul : as) vars = (foldr (mul . eval' vars) (Number 1) as, vars)

-- apply (BuiltIn If : predicate : consequent : [alternative]) vars =
--   case eval' vars predicate of
--     e@(Error _) -> (e, vars)
--     Bool False -> eval vars alternative
--     _ -> eval vars consequent

-- apply [BuiltIn Quote, atom] vars = (atom, vars)

-- apply (BuiltIn Do : as) vars =
--   foldl (\(_, table) item -> eval table item) (Nil, vars) as

-- apply (s@(Symbol _) : as) vars =
--   let x = eval' vars s
--    in case x of
--         b@(BuiltIn _) -> apply (b : as) vars
--         Function f -> proceed vars f as
--         _ -> (Error "Cannot invoke this, as it's not a function", vars)

-- apply (Function f : as) vars =
--   proceed vars f as

-- apply (e@(Error _) : _) vars = (e, vars)

-- apply (l@(List _) : as) vars = apply (fn : as) vars
--   where
--     (fn, _)= eval vars l

-- apply _ vars = (Error "Not implemented yet", vars)

-- -- Eval
-- eval :: VarTable -> Atom -> (Atom, VarTable)

-- eval table (List l) = apply l table

-- eval table atom =
--   (,table)
--     ( case atom of
--         Symbol name ->
--           case Map.lookup name table of
--             Nothing -> Error $ "Undefined variable: " <> name
--             Just a -> a
--         n@(Number _) -> n
--         f@(Function _) -> f
--         m@(Macro _) -> m
--         m@(MultiArityFn _) -> m
--         b@(Bool _) -> b
--         e@(Error _) -> e
--         s@(Str _) -> s
--         b@(BuiltIn _) -> b
--         Nil -> Nil
--         List _ -> undefined
--     )

-- eval' x = fst . eval x

-- eval'' x = snd . eval x

-- -- Function functions
-- create :: VarTable -> [Atom] -> (Atom, VarTable)
-- create vars [Symbol name, List parameters, body]
--   | all isSymbol parameters =
--     let extractText :: Atom -> [Text] -> [Text]
--         extractText (Symbol name) list = name : list
--         extractText _ list = list
--         paramNames = foldr extractText [] parameters
--      in (Symbol name, Map.insert name (Function $ Fun {atom = body, isVariadic = False, parameters = Seq.fromList paramNames}) vars)
--   | otherwise = (Error "Every value in the parameter list must be a symbol", vars)
-- create vars _ = (Error "Invalid function arguments", vars)

-- proceed :: VarTable -> Fun -> [Atom] -> (Atom, VarTable)
-- proceed vars Fun {atom, isVariadic = False, parameters} arguments
--   | length arguments == length parameters = eval localTable atom
--   | otherwise =
--     (Error "Wrong number of arguments for function", vars)
--   where
--     localTable = foldr (uncurry Map.insert) vars (zip (toList parameters) arguments)

-- proceed vars Fun {atom, isVariadic, parameters} arguments
--   | length parameters >= length arguments - 1 =  eval localTable atom
--   | otherwise =
--     (Error "Wrong number of arguments for function", vars)
--   where
--     (fixedArgs, variadicArgs) = splitAt (length parameters - 1) arguments
--     fixedTable = foldr (uncurry Map.insert) vars (zip (toList parameters) fixedArgs)
--     localTable = Map.insert "rest" (List variadicArgs) fixedTable

-- lambda :: VarTable -> [Atom] -> (Atom, VarTable)
-- lambda vars [List parameters, body]
--   | all isSymbol parameters =
--     let extractText :: Atom -> [Text] -> [Text]
--         extractText (Symbol name) list = name : list
--         extractText _ list = list
--         paramNames = foldr extractText [] parameters
--      in (Function $ Fun {atom = body, isVariadic = False, parameters = Seq.fromList paramNames}, vars)
--   | otherwise = (Error "Every value in the parameter list must be a symbol", vars)
-- lambda vars _ = (Error "Invalid lambda arguments", vars)
