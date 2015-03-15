module Location where

import System.IO

data Location = Location (FilePath, Int, Int) | End deriving Show

data Locate a = Locate { at :: Location, value :: a } deriving Show