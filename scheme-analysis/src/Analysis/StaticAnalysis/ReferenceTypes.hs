
module Analysis.StaticAnalysis.ReferenceTypes
  ( Ref (..)
  , Quant (..)
  )
where

newtype Ref
  = Ref Int
  deriving ( Show )

instance Eq Ref where
  Ref a == Ref b = a == b

instance Ord Ref where
  Ref a `compare` Ref b = compare a b

newtype Quant
  = Quant
  {  qname :: Int
  } deriving ( Show )

instance Eq Quant where
  Quant { qname = a } == Quant { qname = b } = a == b

instance Ord Quant where
  Quant { qname = a } `compare` Quant { qname = b } = compare a b
