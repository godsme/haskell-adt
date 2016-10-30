{-# LANGUAGE RankNTypes #-}

module TwoType where

-----------------------------------------
-----------------------------------------
type TwoType = forall r. r -> r -> r

one'' :: TwoType
one'' = \x _ -> x

two'' :: TwoType
two'' = \_ y -> y

-----------------------------------------
-- for implementing Eq
-----------------------------------------
twoEq :: TwoType -> TwoType -> Bool
twoEq m n =
  m (\x -> x True  False)
    (\x -> x False True)
    n
