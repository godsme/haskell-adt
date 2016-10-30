{-# LANGUAGE RankNTypes #-}

module ORD
  ( ORD (..)
  , eq
  , lt
  , gt
  , compare'''
  , toOrdering
  , ordToEq
  ) where

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
compare''' :: ThreeType -> ThreeType -> ORD
compare''' m n =
  m (\x -> x eq lt lt)
    (\x -> x gt eq lt)
    (\x -> x gt gt eq)
    n

toOrdering :: ORD -> Ordering
toOrdering (ORD ord) = ord EQ LT GT

ordToEq :: ORD -> BOOL
ordToEq (ORD ord) = ord true false false
