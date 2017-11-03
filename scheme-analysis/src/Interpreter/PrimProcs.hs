
{-# LANGUAGE RankNTypes #-}

module Interpreter.PrimProcs
  ( applyPrimProc
  )
where

import Scheme.Types
import Interpreter.Types

import Control.Monad
import Control.Monad.Except

type PrimProc = SourcePos -> [Value] -> EvalMonad Value

applyPrimProc :: SourcePos -> String -> [Value] -> EvalMonad Value
applyPrimProc sp "+" rands = add sp rands
applyPrimProc sp "*" rands = mul sp rands
applyPrimProc sp "-" rands = sub sp rands
applyPrimProc sp "/" rands = divide sp rands
applyPrimProc sp "sub1" rands = subOne sp rands
applyPrimProc sp "add1" rands = addOne sp rands

applyPrimProc sp "zero?" rands = isZero sp rands
applyPrimProc sp "<" rands = numComp (<) sp rands
applyPrimProc sp ">" rands = numComp (>) sp rands
applyPrimProc sp "<=" rands = numComp (<=) sp rands
applyPrimProc sp ">=" rands = numComp (>=) sp rands

applyPrimProc sp "not" rands = notScheme sp rands
applyPrimProc sp "and" rands = andScheme sp rands
applyPrimProc sp "or" rands = orScheme sp rands

applyPrimProc sp "car" rands = car sp rands
applyPrimProc sp "cdr" rands = cdr sp rands
applyPrimProc sp "cadr" rands = composeTwo car cdr sp rands

applyPrimProc _ "list" rands = return $ VList rands

applyPrimProc sp "display" rands = display sp rands
applyPrimProc sp "newline" rands = newline sp rands
applyPrimProc sp p _ =
  error $ "PrimProc not yet defined at " ++ show sp ++ ": " ++ p

composeTwo :: PrimProc -> PrimProc -> PrimProc
composeTwo f f' sp vals = f' sp vals >>= \v -> f sp [v]

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
addTwo sp (VConst (SNum _)) v = typeError sp v
addTwo sp (VConst (SInt _)) v = typeError sp v
addTwo sp v (VConst (SNum _)) = typeError sp v
addTwo sp v (VConst (SInt _)) = typeError sp v
addTwo sp v _ = typeError sp v

add :: PrimProc
add sp = foldM (addTwo sp) (VConst $ SInt 0)

addOne :: SourcePos -> [Value] -> EvalMonad Value
addOne sp vals =
  case vals of
    [val] ->
      add sp [val, VConst (SInt 1)]
    _ ->
      wrongNumArgs sp


subTwo :: SourcePos -> Value -> Value -> EvalMonad Value
subTwo _ (VConst (SInt x)) (VConst (SInt y)) = returnInt $ x - y
subTwo _ (VConst (SNum x)) (VConst (SInt y)) = returnNum $ x - fromInteger y
subTwo _ (VConst (SInt x)) (VConst (SNum y)) = returnNum $ fromInteger x - y
subTwo _ (VConst (SNum x)) (VConst (SNum y)) = returnNum $ x - y
subTwo sp (VConst (SNum _)) v = typeError sp v
subTwo sp (VConst (SInt _)) v = typeError sp v
subTwo sp v (VConst (SNum _)) = typeError sp v
subTwo sp v (VConst (SInt _)) = typeError sp v
subTwo sp v _ = typeError sp v

sub :: PrimProc
sub sp vals =
  case vals of
    [] ->
      wrongNumArgs sp
    [VConst (SInt x)] ->
      return (VConst (SInt (-x)))
    [VConst (SNum x)] ->
      return (VConst (SNum (-x)))
    (f:rest) ->
      foldM (subTwo sp) f rest

subOne :: PrimProc
subOne sp vals =
  case vals of
    [val] ->
      sub sp [val, VConst (SInt 1)]
    _ ->
      wrongNumArgs sp

isZero :: PrimProc
isZero sp vals =
  case vals of
    [VConst (SInt 0)] ->
      return (VConst (SBool True))
    [VConst (SNum 0)] ->
      return (VConst (SBool True))
    [VConst (SNum _)] ->
      return (VConst (SBool False))
    [VConst (SInt _)] ->
      return (VConst (SBool False))
    [v] ->
      typeError sp v
    _ ->
      wrongNumArgs sp

mulTwo :: SourcePos -> Value -> Value -> EvalMonad Value
mulTwo _ (VConst (SInt y)) (VConst (SInt x)) = returnInt $ x * y
mulTwo _ (VConst (SNum y)) (VConst (SInt x)) = returnNum $ fromInteger x * y
mulTwo _ (VConst (SInt y)) (VConst (SNum x)) = returnNum $ x * fromInteger y
mulTwo _ (VConst (SNum y)) (VConst (SNum x)) = returnNum $ x * y
mulTwo sp (VConst (SNum _)) v = typeError sp v
mulTwo sp (VConst (SInt _)) v = typeError sp v
mulTwo sp v (VConst (SNum _)) = typeError sp v
mulTwo sp v (VConst (SInt _)) = typeError sp v
mulTwo sp v _ = typeError sp v

mul :: PrimProc
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
    (VConst (SNum _), v) ->
      typeError sp v
    (VConst (SInt _), v) ->
      typeError sp v
    (v, VConst (SInt _)) ->
      typeError sp v
    (v, VConst (SNum _)) ->
      typeError sp v
    (v, _) ->
      typeError sp v
divide :: SourcePos -> [Value] -> EvalMonad Value
divide sp [] = wrongNumArgs sp
divide sp vals = foldM (divTwo sp) (VConst $ SInt 1) vals

notScheme :: PrimProc
notScheme sp vals =
  case vals of
    [VConst (SBool False)] ->
      return (VConst (SBool True))
    [_] ->
      return (VConst (SBool False))
    _ ->
      wrongNumArgs sp

andScheme :: PrimProc
andScheme _ [] = return (VConst (SBool True))
andScheme _ (v@(VConst (SBool False)):_) = return v
andScheme sp (_:rest) = andScheme sp rest

orScheme :: PrimProc
orScheme _ [] = return (VConst (SBool False))
orScheme _ (v@(VConst (SBool True)):_) = return v
orScheme sp (_:rest) = orScheme sp rest


printConstant :: Constant -> String
printConstant (SStr s) = s
printConstant (SBool True) = "#t"
printConstant (SBool False) = "#f"
printConstant (SInt i) = show i
printConstant (SNum i) = show i
printConstant (SSymbol s) = s
printConstant SVoid = "<void>"

printValue :: Value -> EvalMonad String
printValue (VConst c) = return $ printConstant c
printValue (VProc _) = return "<closure>"
printValue (VList vs) =
  mapM printValue vs >>= \ss -> return $ "(" ++ unwords ss ++ ")"
printValue VUndefined = undefined

display :: PrimProc
display sp vals =
  case vals of
    [v] -> do
      s <- printValue v
      liftIO (putStr s)
      return (VConst SVoid)
    _ ->
      wrongNumArgs sp

newline :: PrimProc
newline sp [] = display sp [VConst (SStr "\n")]
newline sp _ = wrongNumArgs sp

car :: SourcePos -> [Value] -> EvalMonad Value
car _ [VList (v:_)] = return v
car sp [VList []] = valueError sp
car sp _ = wrongNumArgs sp

cdr :: SourcePos -> [Value] -> EvalMonad Value
cdr _ [VList (_:v)] = return $ VList v
cdr sp [VList []] = valueError sp
cdr sp _ = wrongNumArgs sp

groupTwos :: [a] -> [(a, a)]
groupTwos (a:b:rest) = (a, b) : groupTwos (b:rest)
groupTwos _ = []

compTwo :: (forall a. Ord a => (a -> a -> Bool))
        -> SourcePos
        -> Value
        -> Value
        -> EvalMonad Bool
compTwo comp _ (VConst (SInt a)) (VConst (SInt b)) =
  return $ a `comp` b
compTwo comp _ (VConst (SNum a)) (VConst (SInt b)) =
  return $ a `comp` fromInteger b
compTwo comp _ (VConst (SInt a)) (VConst (SNum b)) =
  return $ fromInteger a `comp` b
compTwo comp _ (VConst (SNum a)) (VConst (SNum b)) =
  return $ a `comp` b
compTwo _ sp a _ =
  typeError sp a

numComp :: (forall a. Ord a => (a -> a -> Bool))
        -> SourcePos
        -> [Value]
        -> EvalMonad Value
numComp _ sp [] = wrongNumArgs sp
numComp comp sp vals =
  let twos = groupTwos vals
  in do
    bools <- mapM (uncurry $ compTwo comp sp) twos
    return (VConst (SBool (and bools)))
