
import ParseTest
import Test.HUnit
import Control.Monad

main :: IO ()
main = mapM_ runTestTT tests
