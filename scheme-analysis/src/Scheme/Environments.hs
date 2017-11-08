
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Scheme.Environments
  ( extendEnv
  , createEnv
  , putInEnv
  , createEmptyEnv
  , extendGlobalEnv
  , createGlobalEnv
  , putInGlobalEnv
  , createEmptyGlobalEnv
  , globalPairs
  , getAddress
  , getAddressM
  , getEnvValue
  , getEnvValueM
  , getVar
  , getVarM
  , popEnv
  , withNewEnv
  , withExtendedEnv
  , globalReference

  , LocalEnvironment
  , GlobalEnvironment
  , Environment (..)
  , LexicalAddress
  )
where

import Data.Map hiding ( foldl )
import Safe ( at )
import Control.Arrow

newtype Depth = Depth Int
              deriving (Show)
newtype Index = Index Int
              deriving (Show)

data LexicalAddress
  = Global String
  | Bound Depth Index
  deriving ( Show )

data LocalEnvironment a
  = Env [(String, a)] (LocalEnvironment a)
  | EmptyEnv
  deriving ( Show )

instance Functor LocalEnvironment where
  fmap _ EmptyEnv = EmptyEnv
  fmap f (Env m parent) = Env (fmap (second f) m) (fmap f parent)

newtype GlobalEnvironment a = GlobalEnv (Map String a)
  deriving ( Show )

instance Functor GlobalEnvironment where
  fmap f (GlobalEnv m) = GlobalEnv (fmap f m)

class (Monad m) => Environment m a | m -> a where
  getLocalEnv  :: m (LocalEnvironment a)
  getGlobalEnv :: m (GlobalEnvironment a)
  putLocalEnv  :: LocalEnvironment a -> m ()
  putGlobalEnv :: GlobalEnvironment a -> m ()

putInEnv :: (Environment m a)
         => String
         -> a
         -> LexicalAddress
         -> m ()
putInEnv name datum (Bound (Depth 0) (Index i)) = do
  (Env ls parent) <- getLocalEnv
  putLocalEnv $ Env (take (i-1) ls ++ [(name, datum)] ++ drop (i+1) ls) parent
putInEnv name datum (Bound (Depth i) idx) =
  putInEnv name datum (Bound (Depth $ i - 1) idx)
putInEnv n datum (Global _) = do
  (GlobalEnv m) <- getGlobalEnv
  putGlobalEnv (GlobalEnv $ insert n datum m)

popEnv :: Environment m a => m ()
popEnv = do
  (Env _ p) <- getLocalEnv
  putLocalEnv p
  return ()

withNewEnv :: Environment m a
           => [(String, a)]
           -> LocalEnvironment a
           -> m a
           -> m a
withNewEnv ps env task = do
  curr <- getLocalEnv
  putLocalEnv env
  extendEnv ps
  result <- task
  putLocalEnv curr
  return result

extendEnv :: Environment m a
          => [(String, a)]
          -> m ()
extendEnv m = do
  l <- getLocalEnv
  putLocalEnv $ Env m l

withExtendedEnv :: Environment m a
                => [(String, a)]
                -> m b
                -> m b
withExtendedEnv m action = do
  extendEnv m
  val <- action
  popEnv
  return val


extendGlobalEnv :: Environment m a
                => [(String, a)]
                -> m ()
extendGlobalEnv newVals = do
  (GlobalEnv m) <- getGlobalEnv
  putGlobalEnv (GlobalEnv $ foldl (flip $ uncurry insert) m newVals)

putInGlobalEnv :: Environment m a
               => String
               -> a
               -> m ()
putInGlobalEnv name val = do
  (GlobalEnv m) <- getGlobalEnv
  putGlobalEnv (GlobalEnv $ insert name val m)


createEnv :: (Monad m) => [(String, a)] -> m (LocalEnvironment a)
createEnv m =
  return $ Env m EmptyEnv

createEmptyEnv :: LocalEnvironment a
createEmptyEnv = EmptyEnv

createGlobalEnv :: [(String, a)] -> GlobalEnvironment a
createGlobalEnv = GlobalEnv . fromList

createEmptyGlobalEnv :: GlobalEnvironment a
createEmptyGlobalEnv = GlobalEnv $ fromList []

globalPairs :: GlobalEnvironment a -> [(String, a)]
globalPairs (GlobalEnv m) = toList m

getAddress :: String
           -> LocalEnvironment a
           -> GlobalEnvironment a
           -> Maybe LexicalAddress
getAddress iden local (GlobalEnv global) =
  case addrDepth iden 0 local of
    Nothing ->
      case Data.Map.lookup iden global of
        Nothing -> Nothing
        Just _ -> Just (Global iden)
    Just (_, addr) -> Just addr

getAddressM :: Environment m a => String -> m (Maybe LexicalAddress)
getAddressM iden = do
  l <- getLocalEnv
  g <- getGlobalEnv
  return $ getAddress iden l g

addrDepth :: String
          -> Int
          -> LocalEnvironment a
          -> Maybe (a, LexicalAddress)
addrDepth iden d (Env ls parent) =
  case indexAndName iden 0 ls of
    Just (idx, val) -> Just (val, Bound (Depth d) (Index idx))
    Nothing -> addrDepth iden (d+1) parent
addrDepth _ _ EmptyEnv =
  Nothing

getEnvValue :: LexicalAddress
            -> LocalEnvironment a
            -> GlobalEnvironment a
            -> Maybe a
getEnvValue (Global s) _ (GlobalEnv m) =
  Data.Map.lookup s m
getEnvValue (Bound (Depth 0) (Index i)) (Env ls _) _ =
  let val = snd $ ls `at` i
  in Just val
getEnvValue (Bound (Depth d) idx) (Env _ l) g =
  getEnvValue (Bound (Depth (d - 1)) idx) l g
getEnvValue _ _ _ =
  error "Go yell at Coleman for messing up the addressing"

getEnvValueM :: Environment m a
             => LexicalAddress
             -> m (Maybe a)
getEnvValueM addr = do
  lEnv <- getLocalEnv
  gEnv <- getGlobalEnv
  return $ getEnvValue addr lEnv gEnv

indexAndName :: Eq a => a -> Int -> [(a, b)] -> Maybe (Int, b)
indexAndName _ _ [] = Nothing
indexAndName name i ((x, val):rest) =
  if name == x
  then Just (i, val)
  else indexAndName name (i+1) rest

getVar :: String
       -> LocalEnvironment a
       -> GlobalEnvironment a
       -> Maybe (a, LexicalAddress)
getVar iden l g = do
  addr <- getAddress iden l g
  val <- getEnvValue addr l g
  return (val, addr)

getVarM :: Environment m a
         => String
         -> m (Maybe (a, LexicalAddress))
getVarM iden = do
  lenv <- getLocalEnv
  genv <- getGlobalEnv
  return $ getVar iden lenv genv

-- addDepth :: LexicalAddress -> LexicalAddress
-- addDepth (Global _) = Bound (Depth 0) (Index 0)
-- addDepth (Bound (Depth d) i) = Bound (Depth (d+1)) i

globalReference :: String -> LexicalAddress
globalReference = Global

-- isGlobal :: LexicalAddress -> Bool
-- isGlobal (Global _) = True
-- isGlobal _ = False
