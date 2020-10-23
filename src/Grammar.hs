module Grammar where

import Data.Complex
import Data.Matrix

data Command = 
  --  let a =              [1, 0]
  InitQ { qName :: String, qVal :: QBit} |
  InitG { gName :: String, gVal :: Gate} | 
  Measure QBit |
  Return QBit

data QBit =
  QRef String | QArr [Complex Float] | App Gate QBit 

data Gate = 
  GRef String | GMatrix (Matrix (Complex Float)) | Tensor Gate Gate | Product Gate Gate

