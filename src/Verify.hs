
{-# LANGUAGE DeriveFunctor #-}

module Verify where

import Message
import Location

type Verify a = Either (Locate Message) a

(>.>) :: Monad m => (t -> m a) -> (t -> m b) -> (t -> m b)
(f >.> g) x = f x >> g x

(>>=:) :: Monad m => m a -> [a -> m a] -> m a
e >>=: [] = e
e >>=: (c : cs) = (e >>= c) >>=: cs

assert :: Bool -> Locate Message -> Verify ()
assert v msg
	|not v = Left msg
	|otherwise = return ()

select :: (a -> Maybe b) -> [a] -> Maybe b
select f l = go $ map f l where
	go [] = Nothing
	go (Just x : _) = Just x
	go (_ : xs) = go xs