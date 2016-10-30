{-# LANGUAGE RankNTypes #-}

module PAIR
  ( PAIR (..)
  , pair
  , prj1
  , prj2 ) where

----------------------------------------------------------
-- Pair Type
----------------------------------------------------------
newtype PAIR a b =
  PAIR { runPair :: forall r. (a -> b -> r) -> r }

----------------------------------------------------------
-- Data Constructors
----------------------------------------------------------
pair :: a -> b -> PAIR a b
pair x y = PAIR $ \f -> f x y

----------------------------------------------------------
-- Operators
----------------------------------------------------------
prj1 :: PAIR a b -> a
prj1 (PAIR p) = p $ \x _ -> x

prj2 :: PAIR a b -> b
prj2 (PAIR p) = p $ \_ y -> y

----------------------------------------------------------
-- Type Class Instances
----------------------------------------------------------
instance (Show a, Show b) => Show (PAIR a b) where
  show (PAIR p) = "(" ++ p f ++ ")"
    where f a b = show a ++ "," ++ show b

instance (Eq a, Eq b) => Eq (PAIR a b) where
  m == n = (prj1 m == prj1 n) && (prj2 m == prj2 n)
