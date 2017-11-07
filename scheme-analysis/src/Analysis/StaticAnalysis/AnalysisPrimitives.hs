
module Analysis.StaticAnalysis.AnalysisPrimitives where

import Scheme.Types
import Analysis.StaticAnalysis.Types

import Data.Set

type ArgVerifier = [Set Type] -> Either (Set Type) (Set Type)

-- | Helpers

allSubset :: [Set Type] -> Set Type -> Bool
allSubset ls s =
  all (`isSubsetOf` s) ls

-- | Verifiers

allType :: Type -> ArgVerifier
allType t args
  | allSubset args (singleton t) =
    Right $ singleton Numeric
  | otherwise =
    Left $ singleton (Error TypeError)

allNumeric :: ArgVerifier
allNumeric = allType Numeric

allList :: ArgVerifier
allList = allType List


atLeastOne :: ArgVerifier
atLeastOne [] =  Left $ singleton (Error WrongNumArgs)
atLeastOne _ =  Right empty

returnNumeric :: ArgVerifier
returnNumeric _ = Right (singleton Numeric)

returnBool :: ArgVerifier
returnBool _ = Right (singleton Boolean)

returnList :: ArgVerifier
returnList _ = Right (singleton List)

-- | Creating the final result

eitherOr :: Either (Set a) (Set a) -> Set a
eitherOr (Left v) = v
eitherOr (Right v) = v

constructPrimitive :: [ArgVerifier] -> [Set Type] -> Set Type
constructPrimitive verifiers args =
  eitherOr $ do eithers <- mapM ($ args) verifiers
                return $ last eithers

-- | The actual map

type Primitive = [Set Type] -> Set Type

allNum :: Primitive
allNum = constructPrimitive [ allNumeric
                            , returnNumeric
                            ]

atLeastOneAllNum :: Primitive
atLeastOneAllNum = constructPrimitive [ atLeastOne
                                      , allNumeric
                                      , returnNumeric
                                      ]

atLeastOneNumToBool :: Primitive
atLeastOneNumToBool = constructPrimitive [ atLeastOne
                                         , allNumeric
                                         , returnBool
                                         ]

allListsToList :: Primitive
allListsToList = constructPrimitive [ allList
                                    , returnList
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

convertPrimitive ("append", _) args = allListsToList args
convertPrimitive ("list", _) _ = singleton List
convertPrimitive (name, _) _ =
  error $ "Analysis primitive not yet defined: " ++ name
