
module Verify.Type.Form where

import Parse.Type
import Verify
import Location


data Form
	= FormOf TypeField
	| Arrow Form Form
	| Concrete
	deriving Show

data Equiv = Equiv TypeField Form
	deriving Show

generate :: Type -> [Equiv]
generate field'
	|null args = []
	|otherwise = (Equiv (TypeField (typeName field) []) $ arrowBuild args) : concat (map generate args)
	where
	field = value field'
	args = typeArguments field
	arrowBuild [] = FormOf field
	arrowBuild (x:xs) = Arrow (FormOf $ value x) $ arrowBuild xs

generateIs :: Type -> Type -> [Equiv]
generateIs thing is = Equiv (value thing) (FormOf $ value is) : generate thing

formOfEqual :: Form -> Form -> Bool
formOfEqual (FormOf a) (FormOf b) = go a b where
	go x y
		|value (typeName x) /= value (typeName y) = False
		|length xa /= length ya = False
		|otherwise = and $ zipWith go (map value xa) (map value ya)
		where
		xa = typeArguments x
		ya = typeArguments y
formOfEqual _ _ = False
