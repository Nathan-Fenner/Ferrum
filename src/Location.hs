module Location where

data Location m = Location m | End deriving Show

data Locate m a = Locate { at :: Location m, value :: a } deriving Show