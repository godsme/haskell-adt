{-# LANGUAGE RankNTypes #-}

module BOOL
  ( BOOL (..)
  , false
  , true
  , toBool
  , if_then_else
  ) where

import TwoType

----------------------------------------------------------
-- Bool Type
----------------------------------------------------------
newtype BOOL = BOOL { unBool :: TwoType }

----------------------------------------------------------
-- Constructors
----------------------------------------------------------
true :: BOOL
true  = BOOL two_1

false :: BOOL
false = BOOL two_2

----------------------------------------------------------
-- Operators
----------------------------------------------------------
if_then_else :: forall r. BOOL -> r -> r -> r
if_then_else (BOOL b) t f = b t f

not' :: BOOL -> BOOL
not' (BOOL b) = BOOL $ \t f -> b f t

and' :: BOOL -> BOOL -> BOOL
and' (BOOL l) (BOOL r) = BOOL $ \t f -> l (r t f) f

or' :: BOOL -> BOOL -> BOOL
or' (BOOL l) (BOOL r)  = BOOL $ \t f -> l t (r t f)

----------------------------------------------------------
-- Type Class Instances
----------------------------------------------------------
instance Show BOOL where
  show (BOOL b) = b "true" "false"

instance Eq BOOL where
  (BOOL m) == (BOOL n) = twoEq m n

----------------------------------------------------------
-- Utilities
----------------------------------------------------------
toBool :: BOOL -> Bool
toBool (BOOL b) = b True False
