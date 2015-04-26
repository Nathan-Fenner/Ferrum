module Location where

data Location = Location (FilePath, Int, Int) | End FilePath | Special String deriving (Show, Eq)

data Locate a = Locate { at :: Location, value :: a } deriving Show

instance Eq a => Eq (Locate a) where
	x == y = value x == value y

type Name = Locate String

displayLocation :: Location -> String
displayLocation (Location (file, line, col)) = file ++ "/" ++ show line ++ ":" ++ show col
displayLocation (End file) = file ++ "/end"
displayLocation (Special s) = "<" ++ s ++ ">"
