
module Analysis.StaticAnalysis.Patterns.Helpers where

import Analysis.StaticAnalysis.Types
import Data.Set as S

primWithName :: String -> Type -> Bool
primWithName name (Closure (StaticPrimitive name' _)) = name == name'
primWithName _ _ = False

isOnly :: Show a => Set a -> (a -> Bool) -> Bool
isOnly s f =
  let res = S.filter f s
  in S.size s == S.size res && S.size s > 0

aProc :: Type -> Bool
aProc (Closure _) = True
aProc _ = False
