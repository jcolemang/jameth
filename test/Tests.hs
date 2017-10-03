
import Parse.ParseConstants
import Parse.ParseQuote

import Test.HUnit

main :: IO ()
main = mapM_ runTestTT
       ( Parse.ParseConstants.tests
       ++ Parse.ParseQuote.tests
       )
