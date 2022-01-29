module Fuba.Compiler where

import           GHC.Real                       ( Ratio((:%)) )
import           Pipoquinha.BuiltIn
import           Pipoquinha.Function     hiding ( Function )
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp
import qualified Pipoquinha.Type               as Type
import           Protolude

compile :: SExp.T -> Text
compile = \case
    String   txt            -> "\"" <> txt <> "\""
    Number   (num :% 1    ) -> show num
    Number   (num :% denom) -> "(" <> show num <> "/" <> show denom <> ")"
    Bool     True           -> "true"
    Bool     False          -> "false"
    Symbol   txt            -> txt
    Macro    t              -> expand t
    Function t              -> ""
    Error    t              -> ""
    BuiltIn  t              -> ""
    Pair     (List l )      -> compileApplication l
    Pair     (_ :.: _)      -> "throw new Error('Cannot apply pair')"
    Pair     _              -> "return null"

expand = undefined

compileApplication :: [SExp.T] -> Text
compileApplication = \case
    (BuiltIn Add : arguments) ->
        "builtinAdd("
            <> (mconcat . intersperse "," . fmap compile $ arguments)
            <> ")"
    (Symbol "+" : arguments) ->
        "builtinAdd("
            <> (mconcat . intersperse "," . fmap compile $ arguments)
            <> ")"
    (Symbol "*" : arguments) ->
        "builtinMul("
            <> (mconcat . intersperse "," . fmap compile $ arguments)
            <> ")"
    (Symbol "eq?" : arguments) ->
        "builtinEq("
            <> (mconcat . intersperse "," . fmap compile $ arguments)
            <> ")"
    [Symbol "if", predicate, consequent, alternative] ->
        compile predicate
            <> " ? "
            <> compile consequent
            <> " : "
            <> compile alternative
    [Symbol "def", Symbol name, value] ->
        "let " <> name <> " = " <> compile value
    [BuiltIn Defn, Symbol name, Pair (List names), body]
        | Just names <- traverse extractSymbol names
        -> "function "
            <> name
            <> "("
            <> (mconcat . intersperse ",") names
            <> ") {"
            <> compile body
            <> "}"
    [Symbol "let", Pair (List values), body]
        | Just (names, values) <- extractPairs values
        -> "(("
            <> (mconcat . intersperse "," $ names)
            <> ") => "
            <> compile body
            <> ")("
            <> (mconcat . intersperse "," . fmap compile $ values)
            <> ")"
    [Symbol "defn", Symbol name, Pair (List names), body]
        | Just names <- traverse extractSymbol names
        -> "function "
            <> name
            <> "("
            <> (mconcat . intersperse ",") names
            <> ") { return "
            <> compile body
            <> " }"
    (Symbol "print" : arguments) ->
        "console.log("
            <> (mconcat . intersperse "," . fmap compile $ arguments)
            <> ")"
    (Symbol function : arguments) ->
        function
            <> "("
            <> (mconcat . intersperse ",") (compile <$> arguments)
            <> ")"
    _ -> ""

extractSymbol :: SExp.T -> Maybe Text
extractSymbol (Symbol sym) = Just sym
extractSymbol _            = Nothing

extractPairs :: [SExp.T] -> Maybe ([Text], [SExp.T])
extractPairs (Symbol name : value : rest) =
    bimap (name :) (value :) <$> extractPairs rest
extractPairs [] = Just ([], [])
extractPairs _  = Nothing

toDeclaration :: (Text, SExp.T) -> Text
toDeclaration (name, value) = "let " <> name <> " = " <> compile value
