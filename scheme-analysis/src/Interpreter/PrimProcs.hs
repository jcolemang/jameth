
module Interpreter.PrimProcs
  ( applyPrimProc
  )
where

import Scheme.Types
import Interpreter.Types

import Control.Monad
import Control.Monad.Except

applyPrimProc :: SourcePos -> String -> [Value] -> EvalMonad Value
applyPrimProc sp "+" rands = add sp rands
applyPrimProc sp "*" rands = mul sp rands
applyPrimProc sp "-" rands = sub sp rands
applyPrimProc sp "/" rands = divide sp rands
applyPrimProc _ _ _ = undefined

returnInt :: Integer -> EvalMonad Value
returnInt i =
  return . Const $ SInt i

returnNum :: Double -> EvalMonad Value
returnNum i =
  return . Const $ SNum i

addTwo :: SourcePos -> Value -> Value -> EvalMonad Value
addTwo _ (Const (SInt y)) (Const (SInt x)) = returnInt $ x + y
addTwo _ (Const (SNum y)) (Const (SInt x)) = returnNum $ fromInteger x + y
addTwo _ (Const (SInt y)) (Const (SNum x)) = returnNum $ x + fromInteger y
addTwo _ (Const (SNum y)) (Const (SNum x)) = returnNum $ x + y
addTwo sp _ _ = typeError sp
add :: SourcePos -> [Value] -> EvalMonad Value
add sp = foldM (addTwo sp) (Const $ SInt 0)

subTwo :: SourcePos -> Value -> Value -> EvalMonad Value
subTwo _ (Const (SInt y)) (Const (SInt x)) = returnInt $ x - y
subTwo _ (Const (SNum y)) (Const (SInt x)) = returnNum $ fromInteger x - y
subTwo _ (Const (SInt y)) (Const (SNum x)) = returnNum $ x - fromInteger y
subTwo _ (Const (SNum y)) (Const (SNum x)) = returnNum $ x - y
subTwo sp _ _ = typeError sp
sub :: SourcePos -> [Value] -> EvalMonad Value
sub sp = foldM (subTwo sp) (Const $ SInt 0)

mulTwo :: SourcePos -> Value -> Value -> EvalMonad Value
mulTwo _ (Const (SInt y)) (Const (SInt x)) = returnInt $ x * y
mulTwo _ (Const (SNum y)) (Const (SInt x)) = returnNum $ fromInteger x * y
mulTwo _ (Const (SInt y)) (Const (SNum x)) = returnNum $ x * fromInteger y
mulTwo _ (Const (SNum y)) (Const (SNum x)) = returnNum $ x * y
mulTwo sp _ _ = typeError sp
mul :: SourcePos -> [Value] -> EvalMonad Value
mul sp = foldM (mulTwo sp) (Const $ SInt 1)

divTwo :: SourcePos -> Value -> Value -> EvalMonad Value
divTwo sp x y =
  if y == (Const $ SInt 0) || y == (Const $ SNum 0)
  then throwError $ ArithError sp
  else case (x, y) of
    (Const (SInt a), Const (SInt b)) ->
      if mod a b == 0
      then returnInt $ div a b
      else returnNum $ fromInteger a / fromInteger b
    (Const (SInt a), Const (SNum b)) ->
      returnNum $ fromInteger a / b
    (Const (SNum a), Const (SInt b)) ->
      returnNum $ a / fromInteger b
    (Const (SNum a), Const (SNum b)) ->
      returnNum $ a / b
    _ ->
      throwError $ TypeError sp
divide :: SourcePos -> [Value] -> EvalMonad Value
divide sp [] = wrongNumArgs sp
divide sp vals = foldM (divTwo sp) (Const $ SInt 1) vals
