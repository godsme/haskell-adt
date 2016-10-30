{-# LANGUAGE RankNTypes, TypeOperators #-}

module TwoType
        ( TwoType
        , two_1
        , two_2
        , twoEq ) where


import SUM
import UNIT

-----------------------------------------
-- Two Type
-----------------------------------------
type TwoType' = UNIT :+: UNIT

two_1' :: TwoType'
two_1' = inl unit

two_2' :: TwoType'
two_2' = inr unit

-----------------------------------------
-- Two Type
-----------------------------------------
type TwoType = forall r. r -> r -> r

from :: TwoType' -> TwoType
from (SUM s) =
  \x y -> s (\(UNIT u) -> u x) (\(UNIT u) -> u y)

-----------------------------------------
-- Constructors
-----------------------------------------
two_1 :: TwoType
two_1 = from two_1'

two_2 :: TwoType
two_2 = from two_2'

-----------------------------------------
-- for implementing Eq
-----------------------------------------
twoEq :: TwoType -> TwoType -> Bool
twoEq m n =
  m (\x -> x True  False)
    (\x -> x False True)
    n
