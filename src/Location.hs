module Location where

data Location = Location (FilePath, Int, Int) | End FilePath deriving Show

data Locate a = Locate { at :: Location, value :: a } deriving Show

type Name = Locate String

displayLocation :: Location -> String
displayLocation (Location (file, line, col)) = file ++ "/" ++ show line ++ ":" ++ show col
displayLocation (End file) = file ++ "/end"
