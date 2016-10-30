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
eq = ORD three_1

lt :: ORD
lt = ORD three_2

gt :: ORD
gt = ORD three_3

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
