
module Parse.ParseQuoteTest where

import Scheme.JLTypes
import Scheme.JLParse

import Test.HUnit
import Data.Either

getFirstForm (Right (JLProgram (x:_))) = Just x
getFirstForm _ = Nothing

getQuoted (JLQuote x _) = Just x
getQuoted _ = Nothing


parseQuote :: Test
parseQuote = TestCase $ do

  let r1 = getQuoted =<< getFirstForm (runJLParse "(quote a)")
  assertEqual "Basic quote" r1 (Just $ JLConst (JLSymbol "a"))

  let r2 = getQuoted =<< getFirstForm (runJLParse "'a")
  assertEqual "Basic quote symbol" r2 (Just $ JLConst (JLSymbol "a"))

  let r3 = getQuoted =<< getFirstForm (runJLParse "''a")
  assertEqual "Quoted quote" r3 (Just $ JLList [ JLConst $ JLSymbol "quote"
                                              , JLConst $ JLSymbol "a"
                                              ])

  let r4 = getQuoted =<< getFirstForm (runJLParse "'(a (b c) d)")
  assertEqual "List quote" r4 (Just $ JLList [ JLConst $ JLSymbol "a"
                                              , JLList
                                                [ JLConst $ JLSymbol "b"
                                                , JLConst $ JLSymbol "c"
                                                ]
                                              , JLConst $ JLSymbol "d"
                                              ])

parseBadQuote :: Test
parseBadQuote = TestCase $ do
  assertBool "Lonely quote" $ isLeft (runJLParse "quote")
  assertBool "Another lonely quote" $ isLeft (runJLParse "'")
  assertBool "Empty quote" $ isLeft (runJLParse "(quote)")


tests :: [Test]
tests =
  [ parseQuote
  , parseBadQuote
  ]
