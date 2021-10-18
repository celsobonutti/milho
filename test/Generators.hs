module Generators where

import           Protolude
import           Test.QuickCheck

newtype Arithmetic
  = Arithmetic Text
  deriving (Eq, Show)

instance Arbitrary Arithmetic where
  arbitrary = Arithmetic <$> elements ["+", "-", "*", "/"]

newtype BoolOp
  = BoolOp Text
  deriving (Eq, Show)

instance Arbitrary BoolOp where
  arbitrary = BoolOp <$> elements ["and", "or", "not"]
