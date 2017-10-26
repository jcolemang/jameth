
module CodeGeneration.Generation where

import Scheme.Types

import Control.Monad

dummyAnn :: Annotation
dummyAnn =
  Ann PrimitiveSource (-1)

addVar :: Form
addVar =
  A dummyAnn $ Var "+" (globalReference "+")

generateAdd :: [Form] -> Form
generateAdd vs =
  let app vals = A dummyAnn (App addVar vals)
  in app vs

data GenerationState
  = GenerationState
  { vars :: [RawForm]
  }

newtype GenerationMonad
  = GenerationMonad
  { runGeneration :: StateT

generateForms :: [Form]
generateForms =
  let vals = [ Value $ Const $ SInt 0
             , Value $ Const $ SInt (-1)
             , Value $ Const $ SInt 1
             , Value $ Const $ SInt 2
             ]
      vars = [ Var "x" (globalReference "x")
             , Var "y" (globalReference "y")
             , Var "z" (globalReference "z")
             ]
      aVals = fmap (A dummyAnn) vals
      combs = join $ fmap (`replicateM` aVals) [0..length vals]
  in fmap generateAdd combs

generatePrograms :: [Program]
generatePrograms =
  fmap (Program . (:[])) generateForms
