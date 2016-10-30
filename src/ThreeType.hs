{-# LANGUAGE RankNTypes #-}

module ThreeType where

---------------------------------------------
type ThreeType = forall r. r -> r -> r -> r

---------------------------------------------
one''' :: ThreeType
one'''   = \x _ _ -> x

two''' :: ThreeType
two'''   = \_ y _ -> y

three''' :: ThreeType
three''' = \_ _ z -> z

---------------------------------------------
threeEq :: ThreeType -> ThreeType -> Bool
threeEq m n =
  m (\x -> x True  False False)
    (\x -> x False True  False)
    (\x -> x False False True)
    n
