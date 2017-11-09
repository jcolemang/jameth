
{-# LANGUAGE OverloadedStrings #-}

module Analysis.StaticAnalysis.FormTypes where

import Scheme.Types hiding ( Closure
                           , Symbol
                           )
import Analysis.StaticAnalysis.ReferenceTypes

import Data.Set as S
import Data.Map as M
import Data.Aeson hiding ( Error
                         )


type AnalysisForm = Annotated AnalysisAnnotation RawAnalysisForm

data AnalysisAnnotation
  = AnalysisAnn
  { sourcePos :: SourcePos
  , label :: Label
  , outTypes :: Set Type
  }
  deriving ( Show )

data AnalysisFormals
  = AnalysisFormals [(String, Ref)]
  | SymbolFormal Ref
  deriving ( Show )

data RawAnalysisForm
  = AnalysisConst Constant
  | AnalysisVar String LexicalAddress Ref
  | AnalysisQuote SList
  | AnalysisLambda Ref AnalysisFormals
                   [AnalysisForm]
  | AnalysisTwoIf Ref AnalysisForm
                  AnalysisForm
                  AnalysisForm
  | AnalysisDefine String Ref AnalysisForm
  | AnalysisApp Ref AnalysisForm [AnalysisForm]
  deriving ( Show )

newtype AnalysisProgram
  = AnalysisProgram [AnalysisForm]
  deriving ( Show )

data ErrorType
  = NotAProcedure
  | TypeError
  | WrongNumArgs
  deriving ( Show
           , Eq
           )

data Error
  = Err ErrorType SourcePos
  deriving ( Show
           , Eq
           )

instance ToJSON Error where
  toJSON (Err err sp) =
    let (srcLine, srcCol) = case sp of
                              SP line col ->
                                (show line, show $ col - 1)
                              PrimitiveSource ->
                                ("Primitive", "Primitive")
                              ExpandedSource ->
                                let s = "Coleman made a mistake"
                                in (s, s)
    in
      object [ "error"
               .= case err of
                    NotAProcedure ->
                      "I don't think that this is a procedure" :: Value
                    TypeError ->
                      "I think that there is an error with the types here"
                    WrongNumArgs ->
                      "I think you used the wrong number of arguments here"

             , "line"   .= srcLine
             , "column" .= srcCol
             ]

instance Ord Error where -- Needed for sets
  _ `compare` _ = EQ

data StaticClosure
  = StaticProc Ref AnalysisFormals
  | StaticPrimitive String (SourcePos -> [Set Type] -> Set Type)

instance Eq StaticClosure where
  StaticPrimitive name _ == StaticPrimitive name' _ = name == name'
  StaticProc ref _ == StaticProc ref' _ = ref == ref'
  _ == _ = False

instance Show StaticClosure where
  show (StaticProc _ _) = "#<closure>"
  show (StaticPrimitive _ _) = "#<closure>"

instance Ord StaticClosure where
  StaticPrimitive name _ `compare` StaticPrimitive name' _ =
    name `compare` name'
  StaticPrimitive _ _ `compare` _ = GT
  _ `compare` StaticPrimitive _ _ = LT

  StaticProc ref _ `compare` StaticProc ref' _ =
    ref `compare` ref'

data ListType
  = ConcreteList [Type]
  | UnknownList
  deriving ( Show )

data Type
  = Top
  | Numeric
  | Void
  | Closure StaticClosure
  | Error Error
  | Str
  | Boolean
  | List ListType
  | Symbol
  | Bottom
  deriving ( Show )

instance Eq Type where
  Top     == Top      = True
  Bottom  == Bottom   = True
  Numeric == Numeric  = True
  Void    == Void     = True
  _       == _        = False

instance Ord Type where -- Needed for sets
  Top `compare` Top = EQ
  Top `compare` _   = GT
  _   `compare` Top = LT

  Numeric `compare` Numeric = EQ
  Numeric `compare` _       = GT
  _       `compare` Numeric = LT

  Void `compare` Void = EQ
  Void `compare` _    = GT
  _    `compare` Void = LT

  Closure a `compare` Closure b = a `compare` b
  Closure _ `compare` _ = GT
  _ `compare` Closure _ = LT

  Error a `compare` Error b = a `compare` b
  Error _ `compare` _       = GT
  _       `compare` Error _ = LT

  Str `compare` Str = EQ
  Str `compare` _   = GT
  _   `compare` Str = LT

  Boolean `compare` Boolean = EQ
  Boolean `compare` _       = GT
  _ `compare` Boolean       = LT

  List (ConcreteList a) `compare` List (ConcreteList b) = a `compare` b
  List (ConcreteList _) `compare` List _                = GT
  List _                `compare` List (ConcreteList _) = LT
  List UnknownList      `compare` List UnknownList      = EQ
  List _                `compare` _                     = GT
  _                     `compare` List _                = LT

  Symbol `compare` Symbol = EQ
  Symbol `compare` _      = GT
  _ `compare` Symbol      = LT

  Bottom `compare` Bottom = EQ

class Monad m => Quantable m where
  typeTable        :: m (Map Quant (Set Type))
  modifyTypeTable  :: (Map Quant (Set Type) -> Map Quant (Set Type)) -> m ()
  valueTable       :: m (Map Ref (Set Quant))
  modifyValueTable :: (Map Ref (Set Quant) -> Map Ref (Set Quant)) -> m ()
  newQuant         :: m Quant
