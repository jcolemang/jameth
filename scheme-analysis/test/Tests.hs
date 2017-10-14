
import Parse.ParseConstantsTest
import Parse.ParseQuoteTest

import Test.HUnit

main :: IO ()
main = mapM_ runTestTT
       ( Parse.ParseConstantsTest.tests
       ++ Parse.ParseQuoteTest.tests
       )
