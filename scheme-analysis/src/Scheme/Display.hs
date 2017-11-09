
module Scheme.Display
  ( displayProgram
  , displayForm
  , displayFormals
  , displayClosure
  , displayConstant
  , displaySList
  )
where

import Scheme.Forms

import Control.Monad

displayProgram :: Program a -> String
displayProgram (Program fs) =
  join $ displayForm <$> fs

displaySList :: SList -> String
displaySList slist =
  let f sl =
        case sl of
          (Symbol x) -> x
          (Constant c) -> displayConstant c
          (SList s) -> unwords (fmap f s)
  in "(quote " ++ f slist ++ ")"

displayForm :: Form a -> String
displayForm (A _ (Const val)) =
  displayConstant val
displayForm (A _ (Var name addr)) =
  name
displayForm (A _ (Quote val)) =
  "(quote " ++ show val ++ ")"
displayForm (A _ (Lambda formals bodies)) =
  let bs = unwords (map displayForm bodies)
      fs = displayFormals formals
  in "(λ " ++ fs ++ " " ++ bs ++ ")"
displayForm (A _ (App f args)) =
  case unwords (map displayForm args) of
    [] -> -- no argument application
      "(" ++ displayForm f ++ ")"
    as ->
      "(" ++ displayForm f ++ " " ++ as ++ ")"
displayForm (A _ (Define name f)) =
  "(define " ++ name ++ " " ++ displayForm f ++ ")\n"

displayForm (A _ (TwoIf test true false)) =
  "(if " ++ displayForm test ++ " " ++
            displayForm true ++ " " ++
            displayForm false ++ ")"

displayFormals :: Formals -> String
displayFormals (SymbolFormal x) =
  x
displayFormals (Formals fs) =
  "(" ++ unwords fs ++ ")"

displayClosure :: Closure ann a -> String
displayClosure Closure {} = "<closure>"
displayClosure Primitive {} = "<primitive>"

displayConstant :: Constant -> String
displayConstant (SStr s) = show s
displayConstant (SBool True) = "#t"
displayConstant (SBool False) = "#f"
displayConstant (SInt num) = show num
displayConstant (SNum num) = show num
displayConstant (SSymbol x) = x
displayConstant SVoid = "#<void>"
