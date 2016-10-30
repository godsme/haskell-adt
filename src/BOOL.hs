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
true = BOOL one''

false :: BOOL
false = BOOL two''

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

----------------------------------------------------------
-- Operators
----------------------------------------------------------
if_then_else :: forall r. BOOL -> r -> r -> r
if_then_else (BOOL b) t f = b t f
