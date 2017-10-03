
module Scheme.JLPrimitiveProcs where

import Scheme.JLTypes

primitiveProcedures :: [(String, JLClosure)]
primitiveProcedures =
  [ ("+",      JLPrimitive "+" $ AnyNum)
  , ("=",      JLPrimitive "=" $ AtLeast 1)
  , ("*",      JLPrimitive "*" $ AnyNum)
  , ("-",      JLPrimitive "-" $ AtLeast 1)
  , ("/",      JLPrimitive "/" $ AtLeast 1)
  , (">=",     JLPrimitive ">=" $ AtLeast 1)
  , ("sqrt",   JLPrimitive "sqrt" $ Exactly 1)

  , ("cons",   JLPrimitive "cons" $ Exactly 2)
  , ("car",    JLPrimitive "car" $ Exactly 1)
  , ("cdr",    JLPrimitive "cdr" $ Exactly 1)
  , ("caar",   JLPrimitive "caar" $ Exactly 1)
  , ("cadr",   JLPrimitive "cadr" $ Exactly 1)
  , ("cdar",   JLPrimitive "cdar" $ Exactly 1)
  , ("cddr",   JLPrimitive "cddr" $ Exactly 1)
  , ("length", JLPrimitive "length" $ Exactly 1)
  , ("list",   JLPrimitive "list" $ AnyNum)
  , ("append",   JLPrimitive "append" $ AnyNum)

  , ("equal?", JLPrimitive "equal?" $ Exactly 2)
  , ("zero?",  JLPrimitive "zero?" $ Exactly 1)

  , ("andmap", JLPrimitive "andmap" $ AtLeast 2)
  ]
