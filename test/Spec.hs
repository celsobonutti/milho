{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

import           Canjica.EvalApply              ( eval )
import qualified Canjica.Std                   as Std
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
import           Protolude.Partial              ( foldl1
                                                , foldr1
                                                )
import           System.Directory               ( getCurrentDirectory )
import           System.FilePath.Posix          ( addTrailingPathSeparator )
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Text.Megaparsec         hiding ( State )

execute :: Text -> IO SExp.T
execute input = do
  currentDirectory <- getCurrentDirectory <&> addTrailingPathSeparator

  environment      <- Std.environment currentDirectory

  let expression = parseExpression input

  Environment.runM (catch @"runtimeError" (eval expression) (return . Error))
                   environment

printRational :: Rational -> Text
printRational number =
  show (numerator number) <> "/" <> show (denominator number)

{- Arithmetic tests -}

generateArithmetic :: Arithmetic -> [Rational] -> Text
generateArithmetic op numbers =
  [i|(#{op} #{unwords . fmap printRational $ numbers})|]

propArithmetic op numbers = monadicIO $ do
  result <- run . execute $ generateArithmetic op numbers

  assert $ result == expected
 where
  expected = case (op, numbers) of
    (Add, _       )                     -> Number $ sum numbers
    (Mul, _       )                     -> Number $ product numbers
    (Sub, []) -> Error . NoCompatibleBodies . Just $ "-"
    (Sub, [number])                     -> Number . negate $ number
    (Sub, _       )                     -> Number $ foldl1 (-) numbers
    (Div, []) -> Error . NoCompatibleBodies . Just $ "/"
    (Div, [0]     )                     -> Error DividedByZero
    (Div, first : rest) | 0 `elem` rest -> Error DividedByZero
    (Div, [number]) -> Number $ (denominator number % numerator number)
    (Div, _       )                     -> Number $ foldl1 (/) numbers

generatePow :: Rational -> Integer -> Text
generatePow base power = [i|(call-with-error-handler
                              (pow #{printRational base} #{power})
                              (error-code))|]

propPow base power = monadicIO $ do
  result <- run . execute $ generatePow base power

  assert $ result == expected
 where
  expected | base == 0 && power < 0 = Symbol "negative-exponent"
           | otherwise              = Number $ base ^^ power

testArithmetic = describe "Arithmetic" $ do
  prop "works for every basic arithmetic operation" propArithmetic

  prop "works for pow" propPow

{- Variable definition tests -}

generateDefinition :: Integer -> Text
generateDefinition value = [i|(do
                                (def my-var #{value})
                                (eq? #{value} my-var))|]

propDefinition value = monadicIO $ do
  result <- run . execute $ generateDefinition value

  assert $ result == Bool True

generateLet :: Integer -> Integer -> Integer -> Text
generateLet x y z = [i|(let (x #{x} y #{y} z #{z})
                            (+ x y z))|]

propLet x y z = monadicIO $ do
  result <- run . execute $ generateLet x y z

  assert $ result == expected
  where expected = Number . fromIntegral $ x + y + z

generateObject :: [Integer] -> Text
generateObject numbers = [i|(do
                              (import std/list)
                              (import examples/object)
                              (def inc (inspect add-one))
                              (map inc (list #{unwords . fmap show $ numbers}))
                              (inc 'retrieve))|]

propObject numbers = monadicIO $ do
  result <- run . execute $ generateObject numbers

  assert $ result == expected
  where expected = Number . fromIntegral . length $ numbers

generateMlist :: Integer -> Integer -> Integer -> Text
generateMlist fstInitial fstFinal snd = [i|(do
                                            (import std/mlist)
                                            (def fst (mcons #{fstInitial} '()))
                                            (def snd (mcons #{snd} fst))
                                            (set-mcar! fst #{fstFinal})
                                            (mcar (mcdr snd)))|]

propMlist fstInitial fstFinal snd = monadicIO $ do
  result <- run . execute $ generateMlist fstInitial fstFinal snd

  assert $ result == expected
  where expected = Number . fromIntegral $ fstFinal

testState = describe "State" $ do
  prop "works for simple definitions"              propDefinition

  prop "works for let (local state)"               propLet

  prop "works for simple objects (local mutation)" propObject

  prop "works for mutable lists"                   propMlist

{- List operation tests -}

generateMap :: [Integer] -> Text
generateMap numbers =
  [i|(list:map add-one '(#{unwords . fmap show $ numbers}))|]

propMap numbers = monadicIO $ do
  result <- run . execute $ generateMap numbers

  assert $ result == expected
  where expected = Pair . List $ fmap (Number . fromIntegral . (+ 1)) numbers

generateFoldr :: [Integer] -> Text
generateFoldr numbers =
  [i|(list:foldr - 0 '(#{unwords . fmap show $ numbers}))|]


propFoldr numbers = monadicIO $ do
  result <- run . execute $ generateFoldr numbers

  assert $ result == expected
  where expected = Number . fromIntegral $ foldr (-) 0 numbers

generateFilter :: Integer -> [Integer] -> Text
generateFilter compared numbers = [i|(list:filter
                                        (fn (x) (> x #{compared}))
                                        '(#{unwords . fmap show $ numbers}))|]

propFilter compared numbers = monadicIO $ do
  result <- run . execute $ generateFilter compared numbers

  assert $ result == expected
 where
  expected =
    Pair . List $ [ Number . fromIntegral $ x | x <- numbers, x > compared ]

testList = describe "List" $ do
  prop "works for map"    propMap

  prop "works for foldr"  propFoldr

  prop "works for filter" propFilter

{- Control flow tests -}

generateIf :: Bool -> Integer -> Integer -> Text
generateIf condition consequent alternative =
  [i|(if #{condition} #{consequent} #{alternative})|]

propIf (Fn2 f) x y = monadicIO $ do
  result <- run . execute $ generateIf (f x y) x y

  assert $ result == expected
  where expected = Number . fromIntegral $ if f x y then x else y

generateGuard :: Integer -> Integer -> Text
generateGuard clauseNumber body = [i|(call-with-error-handler
                                        (guard ((> 20 #{clauseNumber})) #{body})
                                        error-code)|]

propGuard clauseNumber body = monadicIO $ do
  result <- run . execute $ generateGuard clauseNumber body

  assert $ result == expected
 where
  expected | 20 > clauseNumber = Number . fromIntegral $ body
           | otherwise         = Symbol "failed-guard-clause"

generateBoolOp :: BoolOp -> [Bool] -> Text
generateBoolOp op values = [i|(call-with-error-handler
                                (#{op} #{unwords . fmap show $ values})
                                error-code)|]

propBoolOp op values = monadicIO $ do
  result <- run . execute $ generateBoolOp op values

  assert $ result == expected
 where
  expected = case (op, values) of
    (Not, [value]) -> Bool . not $ value
    (Not, _      ) -> Symbol "wrong-number-of-arguments"
    (And, values ) -> Bool $ and values
    (Or , values ) -> Bool $ or values

testControlFlow = describe "Control flow" $ do
  prop "works for if"                     propIf

  prop "works for guard"                  propGuard

  prop "works for all boolean operations" propBoolOp

{- Error raise and handling tests -}

generateUserRaise :: Integer -> Integer -> Text
generateUserRaise first second = [i|(if (> #{first} #{second})
                                 "yayy"
                                 (raise 'invalid-value "Testing raise"))|]


propUserRaise first second = monadicIO $ do
  result <- run . execute $ generateUserRaise first second

  assert $ result == expected
 where
  expected
    | first > second = String "yayy"
    | otherwise = Error
    $ UserRaised { errorCode = "invalid-value", message = "Testing raise" }

testError = describe "Error raise and handling" $ do
  prop "works for user-defined raises" propUserRaise

{- Import testing -}

generateImport :: Integer -> Text
generateImport value = [i|(do
                            (import "./examples/double.milho")
                            (double #{value}))|]

propImport value = monadicIO $ do
  result <- run . execute $ generateImport value

  assert $ result == (Number . fromIntegral $ value * 2)

generateScopedNestedImport :: Integer -> Text
generateScopedNestedImport value = [i|(do
                                        (import examples/test)
                                        (math:double #{value}))|]

propScopedImport value = monadicIO $ do
  result <- run . execute $ generateScopedNestedImport value

  assert $ result == (Number . fromIntegral $ value * 2)

cyclicImport :: Text
cyclicImport =
  "(call-with-error-handler \
                  \ (import examples/cyclic) \
                  \ error-code)"

testImports = describe "Imports" $ do
  prop "works when using filepath" $ do
    propImport

  prop "works when using modules and are scoped" $ do
    propScopedImport

  it "breaks when are cyclic" $ do
    execute cyclicImport `shouldReturn` Symbol "cyclic-import"

main = hspec $ do
  testArithmetic
  testState
  testList
  testControlFlow
  testError
  testImports
