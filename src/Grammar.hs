module Grammar where

import Data.Complex
import Data.Matrix

data Command =
  --  let a =              [1, 0]
  InitQ { qName :: String, qVal :: QBit} |
  InitG { gName :: String, gVal :: Gate} |
  Measure QBit |
  Return QBit deriving (Eq)

instance Show Command where
  show (InitQ n v) = n ++ " := " ++ show v
  show (InitG n v) = n ++ " := " ++ show v
  show (Measure q) = "Measure " ++ show q
  show (Return q) = "Return " ++ show q

data QBit =
  QRef String | QArr [Complex Float] | App Gate QBit deriving (Eq)

instance Show QBit where
  show (QRef s) = "(" ++ s ++ ": QBit)"
  show (QArr a) = show a
  show (App g q) = show g ++ " <- " ++ show q

data Gate =
  GRef String | GMatrix (Matrix (Complex Float)) | Tensor Gate Gate | Product Gate Gate deriving (Eq)

instance Show Gate where
  show (GRef s) = "(" ++ s ++ ": Gate)"
  show (GMatrix _) = "[[]]"
  show (Tensor g1 g2) = show g1 ++ " * " ++ show g2
  show (Product g1 g2) = show g1 ++ " . " ++ show g2
