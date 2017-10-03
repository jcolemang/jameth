
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scheme.JLParsingTypes
  ( modify', modify
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
  = JLVal JLConstant JLSourcePos
  | JLId String JLSourcePos
  | JLSList [JLTree] JLSourcePos
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
  } deriving (Show)

data JLParseError
  = JLParseError JLSourcePos
  | JLUndefinedVariable String JLSourcePos
  | JLInvalidSyntax String JLSourcePos
  deriving (Show)

-- | Formal Syntax

data JLSyntax
  = BuiltIn String (JLTree -> ParseMonad JLForm)

instance Show JLSyntax where
  show (BuiltIn n _) = "#< " ++ n ++ " >"
