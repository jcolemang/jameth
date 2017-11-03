
module Scheme.PrimitiveProcedures
  ( carsAndCdrs
  , primitiveProcedures
  )
where

import Scheme.Types

import Data.List

carsAndCdrs :: [(String, Closure a)]
carsAndCdrs =
  let allNames = subsequences "aaaadddd"
      valid = filter (not . null) $ filter ((<= 4) . length) allNames
      withCRs = fmap (\x -> "c" ++ x ++ "r") valid
  in flip fmap withCRs $ \x -> (x, Primitive x (Exactly 1))

primitiveProcedures :: [(String, Closure a)]
primitiveProcedures =
  [ ("+",      Primitive "+" AnyNum)
  , ("=",      Primitive "=" $ AtLeast 1)
  , ("*",      Primitive "*" AnyNum)
  , ("-",      Primitive "-" $ AtLeast 1)
  , ("/",      Primitive "/" $ AtLeast 1)
  , (">",     Primitive ">" $ AtLeast 1)
  , ("<",     Primitive "<" $ AtLeast 1)
  , (">=",     Primitive ">=" $ AtLeast 1)
  , ("<=",     Primitive ">=" $ AtLeast 1)
  , ("<=",     Primitive "<=" $ AtLeast 1)
  , ("sqrt",   Primitive "sqrt" $ Exactly 1)
  , ("add1",   Primitive "add1" $ Exactly 1)
  , ("sub1",   Primitive "sub1" $ Exactly 1)

  , ("cons",   Primitive "cons" $ Exactly 2)
  , ("length", Primitive "length" $ Exactly 1)
  , ("list",   Primitive "list" AnyNum)
  , ("append", Primitive "append" AnyNum)

  , ("equal?", Primitive "equal?" $ Exactly 2)
  , ("zero?",  Primitive "zero?" $ Exactly 1)
  , ("not",    Primitive "not" $ Exactly 1)
  , ("or",     Primitive "or" AnyNum)
  , ("and",    Primitive "and" AnyNum)

  , ("andmap", Primitive "andmap" $ AtLeast 2)

  , ("display", Primitive "display" $ Exactly 1)
  , ("newline", Primitive "newline" $ Exactly 0)
  ] ++ carsAndCdrs
