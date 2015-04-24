
module Syntax.Type where

import Location

data Type
	= Type { typeName :: Name, typeArguments :: [Type] }
	deriving Show

instance Eq Type where
	Type nx ax == Type ny ay = value nx == value ny && length ax == length ay && all id (zipWith (==) ax ay)

typeAt :: Type -> Location
typeAt = at . typeName

prettyType :: Type -> String
prettyType (Type name args)
	|null args = value name
	|otherwise = value name ++ "[" ++ commas (map prettyType args) ++ "]"
	where
	commas [] = []
	commas [x] = x
	commas (x:y:t) = x ++ ", " ++ commas (y:t)
