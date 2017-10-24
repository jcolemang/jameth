
module Parse.ParseQuoteTest where

import Scheme.Types
import Scheme.Parse
import Parse.Helpers

import Test.HUnit
import Data.Either

parseQuote :: Test
parseQuote = TestCase $ do
  -- let r1 = getQuoted =<< getFirstForm (runParseNoInit "(quote a)")
  -- assertEqual "Basic quote"
  --             (Just $ Const (SSymbol "a"))
  --             r1

  -- let r2 = getQuoted =<< getFirstForm (runParseNoInit "'a")
  -- assertEqual "Basic quote symbol"
  --             (Just $ Const (SSymbol "a"))
  --             r2

  -- let r3 = getQuoted =<< getFirstForm (runParseNoInit "''a")
  -- assertEqual "Quoted quote"
  --             (Just $ VList [ Const $ SSymbol "quote"
  --                           , Const $ SSymbol "a"
  --                           ])
  --             r3

  -- let r4 = getQuoted =<< getFirstForm (runParseNoInit "'(a (b c) d)")
  -- assertEqual "List quote"
  --             (Just $ VList [ Const $ SSymbol "a"
  --                           , VList
  --                             [ Const $ SSymbol "b"
  --                             , Const $ SSymbol "c"
  --                             ]
  --                           , Const $ SSymbol "d"
  --                           ])
  --             r4
  assertBool "Placeholder" True

parseBadQuote :: Test
parseBadQuote = TestCase $ do
  -- assertBool "Lonely quote" $ isLeft (runParseNoInit "quote")
  -- assertBool "Another lonely quote" $ isLeft (runParseNoInit "'")
  -- assertBool "Empty quote" $ isLeft (runParseNoInit "(quote)")
  assertBool "Placeholder" True


tests :: [Test]
tests =
  [ parseQuote
  , parseBadQuote
  ]
