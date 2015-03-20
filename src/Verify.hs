
{-# LANGUAGE DeriveFunctor #-}

module Verify where

import Parse.Core
import Location

type Verify a = Either (Locate Message) a

(>.>) :: Monad m => (t -> m a) -> (t -> m b) -> (t -> m b)
(f >.> g) x = f x >> g x