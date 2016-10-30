{-# LANGUAGE RankNTypes, TypeOperators #-}

module ThreeType where

import SUM
import UNIT
import BOOL

type ThreeType' = UNIT :+: BOOL

three_1' :: ThreeType'
three_1' = inl unit

three_2' :: ThreeType'
three_2' = inr true

three_3' :: ThreeType'
three_3' = inr false
----------------------------------------------------------
-- Three Type
----------------------------------------------------------
type ThreeType = forall r. r -> r -> r -> r

from :: ThreeType' -> ThreeType
from (SUM s) = \x y z -> s (\(UNIT u) -> u x) (\(BOOL b) -> b y z)

----------------------------------------------------------
-- Constructors
----------------------------------------------------------
three_1 :: ThreeType
three_1   = from three_1'

three_2 :: ThreeType
three_2   = from three_2'

three_3 :: ThreeType
three_3   = from three_3'

----------------------------------------------------------
-- For Eq Type Class
----------------------------------------------------------
threeEq :: ThreeType -> ThreeType -> Bool
threeEq m n =
  m (\x -> x True  False False)
    (\x -> x False True  False)
    (\x -> x False False True)
    n
