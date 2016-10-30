{-# LANGUAGE RankNTypes #-}

module ORD where

import ThreeType
import BOOL

----------------------------------------------------------
-- Ordering Type
----------------------------------------------------------
newtype ORD = ORD { unOrd :: ThreeType }

----------------------------------------------------------
-- Constructors
----------------------------------------------------------
eq :: ORD
eq = ORD one'''

lt :: ORD
lt = ORD two'''

gt :: ORD
gt = ORD three'''

----------------------------------------------------------
-- Type Class Instances
----------------------------------------------------------
instance Show ORD where
  show (ORD m) = m "EQ" "LT" "GT"

instance Eq ORD where
  (ORD m) == (ORD n) = threeEq m n

----------------------------------------------------------
-- Utilities
----------------------------------------------------------
compareThree :: ThreeType -> ThreeType -> ORD
compareThree m n =
  m (\x -> x eq lt lt)
    (\x -> x gt eq lt)
    (\x -> x gt gt eq)
    n

ordToOrdering :: ORD -> Ordering
ordToOrdering (ORD ord) = ord EQ LT GT

ordToEq :: ORD -> BOOL
ordToEq (ORD ord) = ord true false false
