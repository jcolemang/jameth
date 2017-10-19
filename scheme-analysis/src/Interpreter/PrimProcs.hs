
module Interpreter.PrimProcs
  ( applyPrimProc
  )
where

import Scheme.Types
import Interpreter.Types

import Control.Monad

applyPrimProc :: SourcePos -> String -> [Value] -> EvalMonad Value
applyPrimProc sp "+" rands = add sp rands
applyPrimProc sp "*" rands = mul sp rands
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

mulTwo :: SourcePos -> Value -> Value -> EvalMonad Value
mulTwo _ (Const (SInt y)) (Const (SInt x)) = returnInt $ x * y
mulTwo _ (Const (SNum y)) (Const (SInt x)) = returnNum $ fromInteger x * y
mulTwo _ (Const (SInt y)) (Const (SNum x)) = returnNum $ x * fromInteger y
mulTwo _ (Const (SNum y)) (Const (SNum x)) = returnNum $ x * y
mulTwo sp _ _ = typeError sp

mul :: SourcePos -> [Value] -> EvalMonad Value
mul sp = foldM (mulTwo sp) (Const $ SInt 1)
