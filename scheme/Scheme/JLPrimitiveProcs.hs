
module Scheme.JLPrimitiveProcs where

import Scheme.JLTypes

primitiveProcedures :: [(String, Closure)]
primitiveProcedures =
  [ ("+",      Primitive "+"        AnyNum)
  , ("=",      Primitive "="      $ AtLeast 1)
  , ("*",      Primitive "*"        AnyNum)
  , ("-",      Primitive "-"      $ AtLeast 1)
  , ("/",      Primitive "/"      $ AtLeast 1)
  , (">=",     Primitive ">="     $ AtLeast 1)
  , ("sqrt",   Primitive "sqrt"   $ Exactly 1)

  , ("cons",   Primitive "cons"   $ Exactly 2)
  , ("car",    Primitive "car"    $ Exactly 1)
  , ("cdr",    Primitive "cdr"    $ Exactly 1)
  , ("caar",   Primitive "caar"   $ Exactly 1)
  , ("cadr",   Primitive "cadr"   $ Exactly 1)
  , ("cdar",   Primitive "cdar"   $ Exactly 1)
  , ("cddr",   Primitive "cddr"   $ Exactly 1)
  , ("length", Primitive "length" $ Exactly 1)
  , ("list",   Primitive "list"     AnyNum)
  , ("append", Primitive "append"   AnyNum)

  , ("equal?", Primitive "equal?" $ Exactly 2)
  , ("zero?",  Primitive "zero?"  $ Exactly 1)

  , ("andmap", Primitive "andmap" $ AtLeast 2)
  ]
