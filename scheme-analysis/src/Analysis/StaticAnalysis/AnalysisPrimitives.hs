
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
  | True =
    singleton (Error TypeError)
