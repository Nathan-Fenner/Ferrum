
{-# LANGUAGE DeriveFunctor #-}

module Verify where

import Parse.Core
import Control.Applicative

type Verify a = Either Message a