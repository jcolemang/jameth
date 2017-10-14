
module JLExpand where

import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import JLTypes


data ExpansionState
  = NothingYet


data ExpansionError
  = AlsoNothing


data SyntacticEnvironment
  = JLSynEnv
  | JLLocalSynEnv


data SyntacticClosure
  = JLPrimSynClosure
  | JLSynClosure SyntacticEnvironment


-- type Expander m = JLTree -> StateT ExpansionState (ExceptT ExpansionError m) JLForm
