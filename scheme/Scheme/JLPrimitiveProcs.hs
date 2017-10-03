
module Scheme.JLPrimitiveProcs where

import Scheme.JLTypes

primitiveProcedures :: [(String, JLClosure)]
primitiveProcedures =
  [ ("+",      JLPrimitive   AnyNum)
  , ("=",      JLPrimitive $ AtLeast 1)
  , ("*",      JLPrimitive   AnyNum)
  , ("-",      JLPrimitive $ AtLeast 1)
  , ("/",      JLPrimitive $ AtLeast 1)
  , (">=",     JLPrimitive $ AtLeast 1)
  , ("sqrt",   JLPrimitive $ Exactly 1)

  , ("cons",   JLPrimitive $ Exactly 2)
  , ("car",    JLPrimitive $ Exactly 1)
  , ("cdr",    JLPrimitive $ Exactly 1)
  , ("caar",   JLPrimitive $ Exactly 1)
  , ("cadr",   JLPrimitive $ Exactly 1)
  , ("cddr",   JLPrimitive $ Exactly 1)
  , ("cadr",   JLPrimitive $ Exactly 1)
  , ("length", JLPrimitive $ Exactly 1)
  , ("list",   JLPrimitive   AnyNum)

  , ("equal?", JLPrimitive $ Exactly 2)
  , ("zero?",  JLPrimitive $ Exactly 1)

  , ("andmap", JLPrimitive $ AtLeast 2)
  ]
