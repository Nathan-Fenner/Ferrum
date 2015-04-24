
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

-- generics are given
relabelType :: [(Name, Type)] -> Type -> Type
relabelType generics (Type name args) = lookUp name `applyArguments` map (relabelType generics) args
	where
	lookUp :: Name -> Type
	lookUp n = case select (\(match, result) -> if value match == value n then Just result else Nothing) generics of
		Nothing -> Type n []
		Just r -> r
	applyArguments :: Type -> [Type] -> Type
	applyArguments (Type n a) more = Type n (a ++ more)
	select :: (a -> Maybe b) -> [a] -> Maybe b
	select f l = go $ map f l where
		go [] = Nothing
		go (Just x : _) = Just x
		go (_ : xs) = go xs

intType :: Type
intType = Type (Locate (Special "*") "Int") []

stringType :: Type
stringType = Type (Locate (Special "*") "String") []

boolType :: Type
boolType = Type (Locate (Special "*") "Bool") []

voidType :: Type
voidType = Type (Locate (Special "*") "Void") []
