{-# LANGUAGE RankNTypes #-}

module BOT
  ( BOT (..)
  , absurd'
  ) where

----------------------------------------------------------
-- Zero Type
----------------------------------------------------------
type ZeroType = forall r. r

----------------------------------------------------------
-- Bottom Type
----------------------------------------------------------
data BOT = BOT { unBot :: ZeroType }

absurd' :: BOT
absurd' = BOT undefined
