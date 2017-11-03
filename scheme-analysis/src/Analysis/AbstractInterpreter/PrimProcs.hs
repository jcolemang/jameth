
module Analysis.AbstractInterpreter.PrimProcs
  ( applyPrimProc
  , checkMatch
  , getMatch
  , Signature (..)
  )
where

import Analysis.AbstractInterpreter.Types
import Scheme.Types hiding ( Exactly )
import Scheme.PrimitiveProcedures ( carsAndCdrs )

import Control.Monad
import Data.List

import Debug.Trace

-- | Exported Functions

applyPrimProc :: SourcePos
              -> String
              -> [AbstractValue]
              -> AnalysisMonad AbstractValue

applyPrimProc sp "+" vals = abstNumOp sp vals
applyPrimProc sp "-" vals = abstNumOp sp vals
applyPrimProc sp "sub1" vals = abstNumOp sp (mappend vals [AbstNum])
applyPrimProc sp "zero?" vals = isZero sp vals

applyPrimProc sp ">" vals = numComp sp vals
applyPrimProc sp "<" vals = numComp sp vals
applyPrimProc sp "<=" vals = numComp sp vals
applyPrimProc sp ">=" vals = numComp sp vals

applyPrimProc sp "not" vals = abstNot sp vals
applyPrimProc sp "or" vals = abstBoolOp sp vals
applyPrimProc sp "and" vals = abstBoolOp sp vals

applyPrimProc sp "car" vals = carOp sp vals
applyPrimProc sp "cdr" vals = cdrOp sp vals

applyPrimProc sp "list" vals = return $ AbstList vals

applyPrimProc sp "newline" vals = abstNewline sp vals
applyPrimProc sp "display" vals = abstDisplay sp vals

applyPrimProc sp s vals =
  case lookup s carsAndCdrs of
    Nothing -> error $ "Not yet defined: " ++ s
    Just (Primitive name _) -> carOrCdr name sp vals
    Just v -> error ("Invalid operation: " ++ show v)

type PrimProc = SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue

data Signature
  = AnyOf AbstractValue
  | JustOne AbstractValue
  | Exactly [AbstractValue]
  | ExactlyAnd [AbstractValue] Signature
  deriving ( Show
           , Eq
           )

checkMatch :: Signature -> [AbstractValue] -> Bool
checkMatch (AnyOf val) vals = all (== val) vals
checkMatch (JustOne val) vals = [val] == vals
checkMatch (Exactly vals) vals' = vals == vals'
checkMatch (ExactlyAnd vals sig) vals' =
  case stripPrefix vals vals' of
    Nothing -> False
    Just vs -> checkMatch sig vs

getMatch :: [(Signature, AbstractValue)]
         -> [AbstractValue]
         -> Maybe AbstractValue
getMatch sigs vals =
  case dropWhile (not . flip checkMatch vals . fst) sigs of
    [] -> Nothing
    ((_, v):_) -> return v

createPrimProc :: [(Signature, AbstractValue)]
               -> SourcePos
               -> [AbstractValue]
               -> AnalysisMonad AbstractValue
createPrimProc m sp vals =
  case getMatch m vals of
    Nothing -> runtimeError (TypeError vals) sp
    Just v -> return v

numComp :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
numComp =
  createPrimProc [ ( AnyOf AbstNum, AbstBool )
                 ]

composeTwo :: PrimProc -> PrimProc -> PrimProc
composeTwo f f' sp vals = f' sp vals >>= \v -> f sp [v]

carOrCdr :: String -> PrimProc
carOrCdr name =
  case name of
    "car" -> carOp
    "cdr" -> cdrOp
    "caar" -> composeTwo carOp carOp
    "cadr" -> composeTwo carOp cdrOp
    "cdar" -> composeTwo cdrOp carOp
    "cddr" -> composeTwo cdrOp cdrOp

-- | Internal Functions

carOp :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
carOp sp vals =
  case vals of
    [] ->
      runtimeError BadNumArguments sp
    [AbstList []] ->
      runtimeError (ValueError []) sp
    [AbstList (first:_)] ->
      return first
    v@[_] ->
      runtimeError (TypeError v) sp
    _ ->
      runtimeError BadNumArguments sp

cdrOp :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
cdrOp sp vals =
  case vals of
    [] ->
      runtimeError BadNumArguments sp
    [AbstList (_:rest)] ->
      return $ AbstList rest
    v@[_] ->
      runtimeError (TypeError v) sp
    _ ->
      runtimeError BadNumArguments sp

abstNumOp :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
abstNumOp sp = foldM (opTwoNums sp) AbstNum

opTwoNums :: SourcePos
          -> AbstractValue
          -> AbstractValue
          -> AnalysisMonad AbstractValue
opTwoNums sp (Branch a b) x = do
  one <- opTwoNums sp a x
  two <- opTwoNums sp b x
  return $ createBranch one two
opTwoNums sp x (Branch a b) = do
  one <- opTwoNums sp x a
  two <- opTwoNums sp x b
  return $ createBranch one two
opTwoNums _ AbstNum AbstNum =
  return AbstNum
opTwoNums sp a b =
  runtimeError (TypeError [a, b]) sp

isZero :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
isZero _ [AbstNum] = return AbstBool
isZero sp v@[_] = runtimeError (TypeError v) sp
isZero sp _ = runtimeError BadNumArguments sp

abstNot :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
abstNot _ [_] = return AbstBool
abstNot sp _ = runtimeError BadNumArguments sp

abstNewline :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
abstNewline _ [] = return AbstVoid
abstNewline sp _ = runtimeError BadNumArguments sp

abstDisplay :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
abstDisplay _ [_] = return AbstVoid
abstDisplay sp _ = runtimeError BadNumArguments sp

abstBoolOp :: SourcePos -> [AbstractValue] -> AnalysisMonad AbstractValue
abstBoolOp _ vals =
  let twoOp _ _ = AbstBool
  in return $ foldl twoOp AbstBool vals
