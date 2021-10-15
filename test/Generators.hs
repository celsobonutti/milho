module Generators where

import           Protolude
import           Test.QuickCheck

newtype Arithmetic
  = Arithmetic Text
  deriving (Eq, Show)

instance Arbitrary Arithmetic where
    arbitrary = Arithmetic <$> elements ["+", "-", "*", "/"]
