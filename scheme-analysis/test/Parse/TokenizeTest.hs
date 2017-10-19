
module Parse.TokenizeTest where

import Scheme.Tokenize
import Scheme.JLParsingTypes
import Scheme.Types

import Test.HUnit

newtype MostlyTree = M JLTree
  deriving ( Show )

instance Eq MostlyTree where
  (M t1) == (M t2) = mostlyEqual t1 t2

mostlyEqual :: JLTree -> JLTree -> Bool
mostlyEqual (JLVal c _) (JLVal c' _) =
  c == c'
mostlyEqual (JLId x _) (JLId x' _) =
  x == x'
mostlyEqual (JLSList ls _) (JLSList ls' _) =
  and $ fmap (uncurry mostlyEqual) (zip ls ls')
mostlyEqual _ _ =
  False


-- dummy SourcePos
dsp :: SourcePos
dsp = PrimitiveSource

tokenizeTest :: Test
tokenizeTest = TestCase $ do
  let r1 = tokenize "x"
  assertEqual "Basic ID"
              (fmap M <$> Right [JLId "x" dsp])
              (fmap M <$> r1)

  let r2 = tokenize "(x)"
  assertEqual "Basic ID"
              (fmap M <$> Right [JLSList [JLId "x" dsp] dsp])
              (fmap M <$> r2)

tests :: [Test]
tests =
  [ tokenizeTest
  ]
