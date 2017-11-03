
module Parse.TokenizeTest where

import Scheme.Tokenize
import Scheme.ParseTypes
import Scheme.Types

import Test.HUnit

newtype MostlyTree = M Tree
  deriving ( Show )

instance Eq MostlyTree where
  (M t1) == (M t2) = mostlyEqual t1 t2

mostlyEqual :: Tree -> Tree -> Bool
mostlyEqual (TreeVal c _) (TreeVal c' _) =
  c == c'
mostlyEqual (TreeId x _) (TreeId x' _) =
  x == x'
mostlyEqual (TreeSList ls _) (TreeSList ls' _) =
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
              (fmap M <$> Right [TreeId "x" dsp])
              (fmap M <$> r1)

  let r2 = tokenize "(x)"
  assertEqual "Basic ID"
              (fmap M <$> Right [TreeSList [TreeId "x" dsp] dsp])
              (fmap M <$> r2)

tests :: [Test]
tests =
  [ tokenizeTest
  ]
