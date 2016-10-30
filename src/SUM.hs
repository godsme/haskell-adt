{-# LANGUAGE RankNTypes, TypeOperators #-}

module SUM
  ( (:+:)(..)
  , inl
  , inr )
  where

----------------------------------------------------------
-- SUM Type
----------------------------------------------------------
newtype a :+: b =
  SUM { runSum :: forall r. (a -> r) -> (b -> r) -> r }

----------------------------------------------------------
-- Data Constructors
----------------------------------------------------------
inl :: a -> a :+: b
inl x = SUM $ \l _ -> l x

inr :: b -> a :+: b
inr y = SUM $ \_ r -> r y

----------------------------------------------------------
-- Type Class Instances
----------------------------------------------------------
instance (Show a, Show b) => Show (a :+: b) where
  show (SUM s) = s l r
    where l a = "inl " ++ show a
          r b = "inr " ++ show b

instance (Eq a, Eq b) => Eq (a :+: b) where
  (SUM m) == (SUM n) =
    m (\x1 r -> r (\x2 -> x1 == x2) (const False))
      (\y1 r -> r (const False)     (\y2 -> y1 == y2))
      n
