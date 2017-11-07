
module Analysis.StaticAnalysis.AnalysisPrimitives where

import Scheme.Types
import Analysis.StaticAnalysis.Types

import Data.Set

allSubset :: [Set Type] -> Set Type -> Bool
allSubset ls s =
  all (`isSubsetOf` s) ls


convertPrimitive :: (String, Closure ann a) -> [Set Type] -> Set Type
convertPrimitive ("+", _) args
  | allSubset args (singleton Numeric) =
    singleton Numeric
  | otherwise =
    singleton (Error TypeError)
convertPrimitive (name, _) _ =
  error $ "Analysis primitive not yet defined: " ++ name
