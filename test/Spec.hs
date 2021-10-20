{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import           Canjica.EvalApply              ( eval )
import           Canjica.Environment            ( basicEnvironment )
import           Capability.Error
import           Capability.Reader
import           Capability.State
import           Data.FileEmbed
import           Data.IORef
import qualified Data.Map                      as Map
import           Data.Ratio
import           Data.String.Interpolate        ( i )
import           Data.Text                      ( strip )
import           Data.Text.Arbitrary
import           Generators
import           Pipoquinha.Environment         ( CatchCapable
                                                , ReaderCapable
                                                , StateCapable
                                                )
import qualified Pipoquinha.Environment        as Environment
import           Pipoquinha.Error               ( T(..) )
import           Pipoquinha.Parser
import qualified Pipoquinha.SExp               as SExp
import           Pipoquinha.SExp
import           Protolude               hiding ( catch )
import           Protolude.Partial              ( foldl1 )
import           System.Directory               ( getCurrentDirectory )
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Megaparsec         hiding ( State )

basicOps :: ByteString
basicOps = foldr ((<>) . snd) "" $(embedDir "std")

execute :: Text -> IO SExp.T
execute input = do
    case parseFile . decodeUtf8 $ basicOps of
        Left  e        -> return . Error . ParserError $ e
        Right builtIns -> do
            currentDirectory <- getCurrentDirectory
            environment      <- basicEnvironment currentDirectory
            mapM_ (\atom -> Environment.runM (eval atom) environment) builtIns
            let expression = parseExpression input

            Environment.runM
                (catch @"runtimeError" (eval expression) (return . Error))
                environment

generateArithmetic :: Arithmetic -> [Integer] -> Text
generateArithmetic op numbers = [i|(#{op} #{unwords . fmap show $ numbers})|]

prop_Arithmetic op numbers = monadicIO $ do
    result <- run . execute $ generateArithmetic op numbers

    assert $ result == expected
  where
    expected = case (op, numbers) of
        (Add, _) -> Number $ sum (fromIntegral <$> numbers)
        (Mul, _) -> Number $ product (fromIntegral <$> numbers)
        (Sub, []) -> Error . NoCompatibleBodies . Just $ "-"
        (Sub, [number]) -> Number . negate . fromIntegral $ number
        (Sub, _) -> Number $ foldl1 (-) . map fromIntegral $ numbers
        (Div, []) -> Error . NoCompatibleBodies . Just $ "/"
        (Div, [0]) -> Error DividedByZero
        (Div, first : rest) | 0 `elem` rest -> Error DividedByZero
        (Div, [number]) -> Number $ 1 % number
        (Div, _) -> Number $ foldl1 (/) . map fromIntegral $ numbers

generateDefinition :: Integer -> Text
generateDefinition value = [i|(do
                                (def my-var #{value})
                                (eq? #{value} my-var))|]

prop_Definition value = monadicIO $ do
    result <- run . execute $ generateDefinition value

    assert $ result == Bool True

generateLet :: Integer -> Integer -> Integer -> Text
generateLet x y z = [i|(let (x #{x} y #{y} z #{z})
                            (+ x y z))|]

prop_Let x y z = monadicIO $ do
    result <- run . execute $ generateLet x y z

    assert $ result == expected
    where expected = Number . fromIntegral $ x + y + z

generateIf :: Bool -> Integer -> Integer -> Text
generateIf condition consequent alternative =
    [i|(if #{condition} #{consequent} #{alternative})|]

prop_If (Fn2 f) x y = monadicIO $ do
    result <- run . execute $ generateIf (f x y) x y

    assert $ result == expected
    where expected = Number . fromIntegral $ if f x y then x else y

generateMap :: [Integer] -> Text
generateMap numbers =
    [i|(do
        (import (prefix-with list: list))
        (list:map add-one (list #{unwords . fmap show $ numbers})))|]

prop_Map numbers = monadicIO $ do
    result <- run . execute $ generateMap numbers

    assert $ result == expected
    where expected = Pair . List $ fmap (Number . fromIntegral . (+ 1)) numbers

generateObject :: [Integer] -> Text
generateObject numbers = [i|(do
                              (import list)
                              (def inc (inspect add-one))
                              (map inc (list #{unwords . fmap show $ numbers}))
                              (inc 'retrieve))|]

prop_Object numbers = monadicIO $ do
    result <- run . execute $ generateObject numbers

    assert $ result == expected
    where expected = Number . fromIntegral . length $ numbers

generateMlist :: Integer -> Integer -> Integer -> Text
generateMlist fstInitial fstFinal snd = [i|(do
                                            (def fst (mcons #{fstInitial} '()))
                                            (def snd (mcons #{snd} fst))
                                            (set-mcar! fst #{fstFinal})
                                            (mcar (mcdr snd)))|]

prop_Mlist fstInitial fstFinal snd = monadicIO $ do
    result <- run . execute $ generateMlist fstInitial fstFinal snd

    assert $ result == expected
    where expected = Number . fromIntegral $ fstFinal

-- Also tests if call-with-error-handler works
generateGuard :: Integer -> Integer -> Text
generateGuard clauseNumber body = [i|(call-with-error-handler
                                        (guard ((> 20 #{clauseNumber})) #{body})
                                        error-code)|]

prop_Guard clauseNumber body = monadicIO $ do
    result <- run . execute $ generateGuard clauseNumber body

    assert $ result == expected
  where
    expected | 20 > clauseNumber = Number . fromIntegral $ body
             | otherwise         = Symbol "failed-guard-clause"

generateBoolOp :: BoolOp -> [Bool] -> Text
generateBoolOp op values = [i|(call-with-error-handler
                                (#{op} #{unwords . fmap show $ values})
                                error-code)|]

prop_BoolOp op values = monadicIO $ do
    result <- run . execute $ generateBoolOp op values

    assert $ result == expected
  where
    expected = case (op, values) of
        (Not, [value]) -> Bool . not $ value
        (Not, _      ) -> Symbol "wrong-number-of-arguments"
        (And, values ) -> Bool $ and values
        (Or , values ) -> Bool $ or values

generateUserRaise :: Integer -> Integer -> Text
generateUserRaise first second = [i|(if (> #{first} #{second})
                                 "yayy"
                                 (raise 'invalid-value "Testing raise"))|]


prop_UserRaise first second = monadicIO $ do
    result <- run . execute $ generateUserRaise first second

    assert $ result == expected
  where
    expected
        | first > second = String "yayy"
        | otherwise = Error $ UserRaised { errorCode = "invalid-value"
                                         , message   = "Testing raise"
                                         }

generateImport :: Integer -> Text
generateImport value = [i|(do
                            (import "./examples/double.milho")
                            (double #{value}))|]

prop_Import value = monadicIO $ do
    result <- run . execute $ generateImport value

    assert $ result == (Number . fromIntegral $ value * 2)

generateScopedNestedImport :: Integer -> Text
generateScopedNestedImport value = [i|(do
                                        (import examples/test)
                                        (math:double #{value}))|]

prop_ScopedImport value = monadicIO $ do
    result <- run . execute $ generateScopedNestedImport value

    assert $ result == (Number . fromIntegral $ value * 2)

return []

main = $quickCheckAll
