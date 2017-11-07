
module Analysis.StaticAnalysis.AnalysisPrimitives where

import Scheme.Types
import Analysis.StaticAnalysis.Types

import Data.Set
import Control.Monad

type ArgVerifier = [Set Type] -> Either (Set Type) (Set Type)

-- | Helpers

allSubset :: [Set Type] -> Set Type -> Bool
allSubset ls s =
  all (`isSubsetOf` s) ls

-- | Verifiers

allNumeric :: ArgVerifier
allNumeric args
  | allSubset args (singleton Numeric) =
    Right $ singleton Numeric
  | otherwise =
    Left $ singleton (Error TypeError)

atLeastOne :: ArgVerifier
atLeastOne [] =  Left $ singleton (Error WrongNumArgs)
atLeastOne _ =  Right empty

returnNumeric :: ArgVerifier
returnNumeric _ = Right (singleton Numeric)

returnBool :: ArgVerifier
returnBool _ = Right (singleton Boolean)

-- | Creating the final result

eitherOr :: Either (Set a) (Set a) -> Set a
eitherOr (Left v) = v
eitherOr (Right v) = v

constructPrimitive :: [ArgVerifier] -> [Set Type] -> Set Type
constructPrimitive verifiers args =
  eitherOr $ do eithers <- mapM ($ args) verifiers
                return $ last eithers

-- | The actual map

allNum = constructPrimitive [ allNumeric
                            , returnNumeric
                            ]

atLeastOneAllNum = constructPrimitive [ atLeastOne
                                      , allNumeric
                                      , returnNumeric
                                      ]

atLeastOneNumToBool = constructPrimitive [ atLeastOne
                                         , allNumeric
                                         , returnBool
                                         ]

convertPrimitive :: (String, Closure ann a) -> [Set Type] -> Set Type
convertPrimitive ("+", _) args = allNum args
convertPrimitive ("*", _) args = allNum args
convertPrimitive ("-", _) args = allNum args
convertPrimitive ("/", _) args = atLeastOneAllNum args
convertPrimitive ("<", _) args = atLeastOneNumToBool args
convertPrimitive (">", _) args = atLeastOneNumToBool args
convertPrimitive ("<=", _) args = atLeastOneNumToBool args
convertPrimitive (">=", _) args = atLeastOneNumToBool args

convertPrimitive ("list", _) args = undefined
convertPrimitive ("append", _) args = undefined
convertPrimitive (name, _) _ =
  error $ "Analysis primitive not yet defined: " ++ name
