
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scheme.JLParsingTypes
  ( modify', modify
  , getLabel
  , BoundValue (..)
  , JLTree (..)
  , ParseState (..)
  , JLSyntax (..)
  , JLParseError (..)
  , ParseMonad (..)
  )
where

import Scheme.JLTypes

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity

-- | Lexing sort of things

data JLTree
  = JLVal Constant SourcePos
  | JLId String SourcePos
  | JLSList [JLTree] SourcePos
  deriving (Show)

data BoundValue
  = BVal
  | BSyntax JLSyntax
  | EmptySlot
  deriving ( Show )

newtype ParseMonad a
  = ParseMonad
  { runParser :: ExceptT JLParseError (StateT ParseState Identity) a
  } deriving ( Functor, Applicative, Monad,
               MonadState ParseState, MonadError JLParseError )

data ParseState
  = ParseState
  { localEnv :: LocalEnvironment BoundValue
  , globalEnv :: GlobalEnvironment BoundValue
  , labelNum :: Int
  } deriving (Show)

getLabel :: ParseMonad Int
getLabel = do
  l <- labelNum <$> get
  modify $ \s -> s { labelNum = l + 1 }
  return l

data JLParseError
  = JLParseError SourcePos
  | JLUndefinedVariable String SourcePos
  | JLInvalidSyntax String SourcePos
  deriving (Show, Eq)

-- | Formal Syntax

data JLSyntax
  = BuiltIn String (JLTree -> ParseMonad Form)

instance Show JLSyntax where
  show (BuiltIn n _) = "#< " ++ n ++ " >"
