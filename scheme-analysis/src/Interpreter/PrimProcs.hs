
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
  return . VConst $ SInt i

returnNum :: Double -> EvalMonad Value
returnNum i =
  return . VConst $ SNum i

addTwo :: SourcePos -> Value -> Value -> EvalMonad Value
addTwo _ (VConst (SInt y)) (VConst (SInt x)) = returnInt $ x + y
addTwo _ (VConst (SNum y)) (VConst (SInt x)) = returnNum $ fromInteger x + y
addTwo _ (VConst (SInt y)) (VConst (SNum x)) = returnNum $ x + fromInteger y
addTwo _ (VConst (SNum y)) (VConst (SNum x)) = returnNum $ x + y
addTwo sp _ _ = typeError sp
add :: SourcePos -> [Value] -> EvalMonad Value
add sp = foldM (addTwo sp) (VConst $ SInt 0)

subTwo :: SourcePos -> Value -> Value -> EvalMonad Value
subTwo _ (VConst (SInt x)) (VConst (SInt y)) = returnInt $ x - y
subTwo _ (VConst (SNum x)) (VConst (SInt y)) = returnNum $ x - fromInteger y
subTwo _ (VConst (SInt x)) (VConst (SNum y)) = returnNum $ fromInteger x - y
subTwo _ (VConst (SNum x)) (VConst (SNum y)) = returnNum $ x - y
subTwo sp _ _ = typeError sp
sub :: SourcePos -> [Value] -> EvalMonad Value
sub sp vals =
  case vals of
    [] ->
      wrongNumArgs sp
    (f:rest) ->
      foldM (subTwo sp) f rest

mulTwo :: SourcePos -> Value -> Value -> EvalMonad Value
mulTwo _ (VConst (SInt y)) (VConst (SInt x)) = returnInt $ x * y
mulTwo _ (VConst (SNum y)) (VConst (SInt x)) = returnNum $ fromInteger x * y
mulTwo _ (VConst (SInt y)) (VConst (SNum x)) = returnNum $ x * fromInteger y
mulTwo _ (VConst (SNum y)) (VConst (SNum x)) = returnNum $ x * y
mulTwo sp _ _ = typeError sp
mul :: SourcePos -> [Value] -> EvalMonad Value
mul sp = foldM (mulTwo sp) (VConst $ SInt 1)

divTwo :: SourcePos -> Value -> Value -> EvalMonad Value
divTwo sp x y =
  if y == (VConst $ SInt 0) || y == (VConst $ SNum 0)
  then throwError $ ArithError sp
  else case (x, y) of
    (VConst (SInt a), VConst (SInt b)) ->
      if mod a b == 0
      then returnInt $ div a b
      else returnNum $ fromInteger a / fromInteger b
    (VConst (SInt a), VConst (SNum b)) ->
      returnNum $ fromInteger a / b
    (VConst (SNum a), VConst (SInt b)) ->
      returnNum $ a / fromInteger b
    (VConst (SNum a), VConst (SNum b)) ->
      returnNum $ a / b
    _ ->
      throwError $ TypeError sp
divide :: SourcePos -> [Value] -> EvalMonad Value
divide sp [] = wrongNumArgs sp
divide sp vals = foldM (divTwo sp) (VConst $ SInt 1) vals
