{-# LANGUAGE RankNTypes #-}

module NAT
  ( NAT(..)
  , z'
  , s'
  , zero'
  , mult'
  , plus'
  , pred'
  ) where

import ORD
import BOOL

-----------------------------------------
-- Primitive Recursive Type
-----------------------------------------
type PRType =
  forall r. r -> (r -> r) -> r

oneN :: PRType
oneN = \z s -> z

uN :: PRType -> PRType
uN n = \z s -> s (n z s)

-----------------------------------------
eN :: PRType -> PRType
eN n = \z s -> let f k = \m -> m (k s)
        in n (\x -> z) f id

-----------------------------------------
newtype PRTW = PRTW PRType

compareN :: PRType -> PRType -> ORD
compareN m n = fst $ m (zz, PRTW n) comp
  where zz = n eq (const lt)

        comp :: (ORD, PRTW) -> (ORD, PRTW)
        comp r@((ORD ord), x@(PRTW xx)) =
          ord (gt, x) (f (eN xx)) r

        f :: PRType -> (ORD, PRTW)
        f n = n (eq, PRTW n) (const (lt, PRTW n))

eqN :: PRType -> PRType -> BOOL
eqN m n = ordToEq $ compareN m n
-----------------------------------------
newtype NAT = NAT { unNat :: PRType }

z' :: NAT
z' = NAT oneN

s' :: NAT -> NAT
s' (NAT n) = NAT (uN n)

-----------------------------------------
zero' :: NAT -> BOOL
zero' (NAT n) = n true (\x -> false)

mult' :: NAT -> NAT -> NAT
mult' (NAT m) (NAT n) =
  NAT $ \z s -> m z (flip n s)

plus' :: NAT -> NAT -> NAT
plus' (NAT m) (NAT n) =
  NAT $ \z s -> m (n z s) s

pred' :: NAT -> NAT
pred' (NAT n) = NAT (eN n)

-----------------------------------------
instance Show NAT where
  show (NAT n) = show $ n 0 (1+)

instance Eq NAT where
  (NAT m) == (NAT n) =
    toBool $ eqN m n

instance Ord NAT where
  compare (NAT m) (NAT n) =
    ordToOrdering (compareN m n)
