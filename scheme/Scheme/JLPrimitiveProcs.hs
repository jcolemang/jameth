
module Scheme.JLPrimitiveProcs where

import Scheme.JLTypes

primitiveProcedures :: [(String, JLClosure)]
primitiveProcedures =
  [ ("+",      JLPrimitive)
  , ("=",      JLPrimitive)
  , ("*",      JLPrimitive)
  , ("-",      JLPrimitive)
  , ("/",      JLPrimitive)
  , (">=",     JLPrimitive)
  , ("cons",   JLPrimitive)
  , ("equal?", JLPrimitive)
  , ("car",    JLPrimitive)
  , ("cdr",    JLPrimitive)
  , ("length", JLPrimitive)
  , ("sqrt",   JLPrimitive)
  , ("cadr",   JLPrimitive)
  , ("list",   JLPrimitive)
  , ("andmap", JLPrimitive)
  , ("zero?",  JLPrimitive)
  ]
