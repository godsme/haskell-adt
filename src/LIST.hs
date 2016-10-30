{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module LIST
  ( LIST (..)
  , nil
  , cons

  , null'
  , length'
  , head'
  , filter'
  , append'
  , reverse'
  , foldl'
  , foldl''
  , take'
  , drop'
  ) where

import BOOL
import NAT
import UNIT

----------------------------------------------------------
-- List Type
----------------------------------------------------------
newtype LIST a =
  LIST { runList :: forall r. r -> (a -> r -> r) -> r }

----------------------------------------------------------
-- Constructors
----------------------------------------------------------
nil :: LIST a
nil = LIST $ \n c -> n

cons :: a -> LIST a -> LIST a
cons x (LIST xs) = LIST $ \n c -> c x (xs n c)

----------------------------------------------------------
-- Operators
----------------------------------------------------------
null' :: LIST a -> BOOL
null' (LIST xs) = xs true $ \_ _ -> false

----------------------------------------------------------
length' :: LIST a -> NAT
length' (LIST xs) = xs z' $ \_ xs' -> s' xs'

----------------------------------------------------------
head' :: LIST a -> a
head' (LIST xs) = xs undefined $ \x' _ -> x'

----------------------------------------------------------
tail' :: LIST a -> LIST a
tail' (LIST xs) = fst $ xs (nil, nil) f
   where f x' xs' = (snd xs', cons x' (snd xs'))

----------------------------------------------------------
foldr' :: (a -> r -> r) -> r -> LIST a -> r
foldr' acc zero (LIST xs) = xs zero acc

----------------------------------------------------------
filter' :: (a -> BOOL) -> LIST a -> LIST a
filter' pred (LIST xs) = xs nil iter
  where iter x' xs' =
          if_then_else (pred x') (cons x' xs') xs'

----------------------------------------------------------
append' :: LIST a -> LIST a -> LIST a
append' (LIST lxs) rxs =
  lxs rxs $ \hd tl -> cons hd tl

----------------------------------------------------------
reverse' :: LIST a -> LIST a
reverse' (LIST xs) =
  xs nil $ \hd tl -> append' tl $ cons hd nil

----------------------------------------------------------
foldl' :: forall a r. (r -> a -> r) -> r -> LIST a -> r
foldl' acc zero = foldr' (flip acc) zero . reverse'

----------------------------------------------------------
foldl'' :: forall a b. (b -> a -> b) -> b -> LIST a -> b
foldl'' f z0 xs = foldr' f' id' xs z0
     where f' :: a -> (b -> b) -> (b -> b)
           f' x k = \z -> k $ f z x

----------------------------------------------------------
take' :: NAT -> LIST a -> LIST a
take' n xs = reverse' . fst $ foldl'' f (nil, n) xs
  where f :: forall a. (LIST a, NAT) -> a -> (LIST a, NAT)
        f acc@(ys, n') y = (unNat n') acc $ const (cons y ys, pred' n')

----------------------------------------------------------
drop' :: NAT -> LIST a -> LIST a
drop' n xs = reverse' . fst $ foldl'' f (nil, n) xs
  where f :: forall a. (LIST a, NAT) -> a -> (LIST a, NAT)
        f (ys, n') y = (unNat n') (cons y ys, n') $ const (ys, pred' n')

----------------------------------------------------------
-- Type Class instances
----------------------------------------------------------
instance (Show a) => Show (LIST a) where
  show (LIST xs) = enclose '[' ']' . snd . flip xs content $ ("", "")
    where content x' xs' = (", ", show x' ++ fst xs' ++ snd xs')

instance (Eq a) => Eq (LIST a) where
  (LIST x) == (LIST y) = undefined

enclose :: Char -> Char -> String -> String
enclose left right x = left : x ++ [right]
