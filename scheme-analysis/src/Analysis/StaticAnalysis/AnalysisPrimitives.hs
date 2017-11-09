
module Analysis.StaticAnalysis.AnalysisPrimitives where

import Scheme.Types
import Analysis.StaticAnalysis.Types

import Data.Set

type ArgVerifier = SourcePos -> [Set Type] -> Either (Set Type) (Set Type)

-- | Helpers

allSubset :: [Set Type] -> Set Type -> Bool
allSubset ls s =
  all (`isSubsetOf` s) ls

carList :: SourcePos -> Type -> Type
carList _ (List UnknownList) = Top
carList _ (List (ConcreteList (x:_))) = x
carList sp _ = Error (Err TypeError sp)

cdrList :: SourcePos -> Type -> Type
cdrList _ (List UnknownList) = Top
cdrList _ (List (ConcreteList (_:rest))) = List (ConcreteList rest)
cdrList sp _ = Error (Err TypeError sp)

getError :: Set Type -> Maybe Type
getError s =
  let f (x@(Error _):_) = Just x
      f (_:rest) = f rest
      f [] = Nothing
  in f $ toList s

-- | Verifiers

isList :: Type -> Bool
isList (List _) = True
isList _ = False

allType :: Type -> ArgVerifier
allType t sp args
  | allSubset args (singleton t) =
    Right $ singleton Numeric
  | otherwise =
    Left $ singleton (Error (Err TypeError sp))

allMatches :: (Type -> Bool) -> Type -> ArgVerifier
allMatches f deflt sp args =
  if and (fmap (all f . toList) args)
  then return $ singleton deflt
  else return $ singleton (Error (Err TypeError sp))

allNumeric :: ArgVerifier
allNumeric = allType Numeric

allList :: ArgVerifier
allList = allMatches isList Top

atLeastOne :: ArgVerifier
atLeastOne sp [] =  Left $ singleton (Error (Err WrongNumArgs sp))
atLeastOne _ _ =  Right empty

exactlyOne :: ArgVerifier
exactlyOne _ [_] =  Right empty
exactlyOne sp _ = Left $ singleton (Error (Err WrongNumArgs sp))

noErrors :: ArgVerifier
noErrors _ [] = Right (singleton Top)
noErrors sp (s:rest) =
  case getError s of
    Nothing ->
      noErrors sp rest
    Just err ->
      Left $ singleton err

returnNumeric :: ArgVerifier
returnNumeric _ _ = Right (singleton Numeric)

returnBool :: ArgVerifier
returnBool _ _ = Right (singleton Boolean)

dumbList :: ArgVerifier
dumbList _ _ = Right (singleton (List UnknownList))

returnAnything :: ArgVerifier
returnAnything _ _ = Right (singleton Top)

-- | Creating the final result

eitherOr :: Either (Set a) (Set a) -> Set a
eitherOr (Left v) = v
eitherOr (Right v) = v

constructPrimitive :: [ArgVerifier] -> SourcePos -> [Set Type] -> Set Type
constructPrimitive verifiers sp args =
  eitherOr $ do eithers <- mapM (\f -> f sp args) (noErrors:verifiers)
                return $ last eithers

-- | The actual map

type Primitive = SourcePos -> [Set Type] -> Set Type

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

oneNumBool :: Primitive
oneNumBool = constructPrimitive [ exactlyOne
                                , allNumeric
                                , returnBool
                                ]

oneNumNumeric :: Primitive
oneNumNumeric = constructPrimitive [ exactlyOne
                                   , allNumeric
                                   , returnNumeric
                                   ]

oneArgBool :: Primitive
oneArgBool = constructPrimitive [ exactlyOne
                                , returnBool
                                ]

anyArgBool :: Primitive
anyArgBool = constructPrimitive [ returnBool
                                ]

allListsToList :: Primitive
allListsToList = constructPrimitive [ allList
                                    , dumbList
                                    ]

oneListToTop :: Primitive
oneListToTop = constructPrimitive [ exactlyOne
                                  , allList
                                  , returnAnything
                                  ]

oneListToList :: Primitive
oneListToList = constructPrimitive [ exactlyOne
                                   , allList
                                   , dumbList
                                   ]

anyToList :: Primitive
anyToList = constructPrimitive [ dumbList
                               ]


convertPrimitive :: (String, Closure ann a) -> SourcePos -> [Set Type] -> Set Type
convertPrimitive ("+", _) sp args  = allNum sp args
convertPrimitive ("*", _) sp args  = allNum sp args
convertPrimitive ("-", _) sp args  = allNum sp args
convertPrimitive ("/", _) sp args  = atLeastOneAllNum sp  args
convertPrimitive ("<", _) sp args  = atLeastOneNumToBool sp args
convertPrimitive (">", _) sp args  = atLeastOneNumToBool sp args
convertPrimitive ("<=", _) sp args = atLeastOneNumToBool sp args
convertPrimitive (">=", _) sp args = atLeastOneNumToBool sp args
convertPrimitive ("=", _) sp args  = atLeastOneNumToBool sp args
convertPrimitive ("zero?", _) sp args = oneNumBool sp args

convertPrimitive ("sub1", _) sp args = oneNumNumeric sp args

convertPrimitive ("append", _) sp args = allListsToList sp args
convertPrimitive ("list", _) sp args = anyToList sp args
convertPrimitive ("car", _) sp args = oneListToTop sp args
convertPrimitive ("cdr", _) sp args = oneListToList sp args
convertPrimitive ("cadr", _) sp args = oneListToTop sp args
convertPrimitive ("caddr", _) sp args = oneListToTop sp args

convertPrimitive ("not", _) sp args = oneArgBool sp args
convertPrimitive ("and", _) sp args = anyArgBool sp args
convertPrimitive ("or", _) sp args = anyArgBool sp args

convertPrimitive (name, _) _ _ =
  error $ "Analysis primitive not yet defined: " ++ name
