
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Scheme.ParseTypes
  ( modify', modify
  , getLabel
  , initialState
  , expandedAnn
  , BoundValue (..)
  , Tree (..)
  , ParseState ( ParseState, localEnv, globalEnv )
  , Syntax (..)
  , ParseError (..)
  , ParseMonad (..)
  )
where

import Scheme.Types

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Identity
import Data.Aeson
import Data.Text

-- | Lexing sort of things

data BoundValue
  = BVal
  | BSyntax Syntax
  | EmptySlot
  deriving ( Show )

newtype ParseMonad a
  = ParseMonad
  { runParser :: ExceptT ParseError (StateT ParseState Identity) a
  } deriving ( Functor, Applicative, Monad,
               MonadState ParseState, MonadError ParseError
             )

data ParseState
  = ParseState
  { localEnv  :: LocalEnvironment BoundValue
  , globalEnv :: GlobalEnvironment BoundValue
  , labelNum  :: Int
  }
  -- deriving (Show)

instance Environment ParseMonad BoundValue where
  getLocalEnv    = localEnv <$> get
  getGlobalEnv   = globalEnv <$> get
  putLocalEnv l  = modify $ \s -> s { localEnv  = l }
  putGlobalEnv g = modify $ \s -> s { globalEnv = g }

initialState :: GlobalEnvironment BoundValue -> ParseState
initialState g =
  ParseState
  { localEnv = createEmptyEnv
  , globalEnv = g
  , labelNum = 0
  }

getLabel :: ParseMonad Int
getLabel = do
  l <- labelNum <$> get
  modify $ \s -> s { labelNum = l + 1 }
  return l

expandedAnn :: Tree -> ParseMonad Annotation
expandedAnn t = do
  l <- getLabel
  return $ Ann ExpandedSource l t

data ParseError
  = ParseError SourcePos
  | InvalidSyntax String SourcePos
  deriving (Show, Eq)

instance ToJSON ParseError where
  toJSON (ParseError (SP lineNum colNum)) =
    object [ "error" .= String "Parse Error"
           , "line" .= String (pack $ show lineNum)
           , "column" .= String (pack $ show colNum)
           ]
  toJSON (InvalidSyntax name (SP lineNum colNum)) =
    object [ "error" .= String (pack $ "Invalid syntax in " ++ name)
           , "line" .= String (pack $ show lineNum)
           , "column" .= String (pack $ show colNum)
           ]

-- | Formal Syntax

data Syntax
  = BuiltIn String (Tree -> ParseMonad (Form Annotation))

instance Show Syntax where
  show (BuiltIn n _) = "#< " ++ n ++ " >"
