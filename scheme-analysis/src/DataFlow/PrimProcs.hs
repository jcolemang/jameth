
module DataFlow.PrimProcs
  ( applyPrimProc
  )
where

import DataFlow.Types

import Control.Monad

-- | Exported Functions

applyPrimProc :: String -> [AbstractValue] -> AnalysisMonad AbstractValue
applyPrimProc "+" vals = abstNumOp vals
applyPrimProc "-" vals = abstNumOp vals
applyPrimProc _ _ = error "Not yet defined"

-- | Internal Functions

-- abstAdd :: [AbstractValue] -> AnalysisMonad AbstractValue
-- abstAdd = foldM addTwo AbstNum

abstNumOp :: [AbstractValue] -> AnalysisMonad AbstractValue
abstNumOp = foldM opTwoNums AbstNum

opTwoNums :: AbstractValue -> AbstractValue -> AnalysisMonad AbstractValue
opTwoNums (Branch a b) x = do
  one <- opTwoNums a x
  two <- opTwoNums b x
  return $ createBranch one two
opTwoNums x (Branch a b) = do
  one <- opTwoNums x a
  two <- opTwoNums x b
  return $ createBranch one two
opTwoNums AbstNum AbstNum =
  return AbstNum
opTwoNums _ _ =
  return (AbstErr TypeError)

-- addTwo :: AbstractValue -> AbstractValue -> AnalysisMonad AbstractValue
-- addTwo AbstNum AbstNum = return AbstNum
-- addTwo (Branch a b) AbstNum = do
--   one <- addTwo a AbstNum
--   two <- addTwo b AbstNum
--   return $ Branch one two
-- addTwo _ _ = return AbstNum

-- abstSub :: [AbstractValue] -> AnalysisMonad AbstractValue
-- abstSub = foldM subTwo AbstNum

-- subTwo :: AbstractValue -> AbstractValue -> AnalysisMonad AbstractValue
-- subTwo AbstNum AbstNum = return AbstNum
-- subTwo (Branch a b) AbstNum = do
--   one <- subTwo a AbstNum
--   two <- subTwo b AbstNum
--   return $ Branch one two
-- subTwo _ _ = return AbstNum
