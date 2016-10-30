{-# LANGUAGE RankNTypes #-}

module TRIPLE where

import PAIR

-----------------------------------------
-- Internal Triple Type
-----------------------------------------
type TRIPLE a b c = PAIR a (PAIR b c)

triple :: a -> b -> c -> TRIPLE a b c
triple x y z = pair x (pair y z)

tri_1 :: TRIPLE a b c -> a
tri_1 = prj1

tri_2 :: TRIPLE a b c -> b
tri_2 = prj1 . prj2

tri_3 :: TRIPLE a b c -> c
tri_3 = prj2 . prj2
