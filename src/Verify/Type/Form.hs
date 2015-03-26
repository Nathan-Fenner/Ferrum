
module Verify.Type.Form where

import Parse.Type
import Verify
import Location


data Form
	= FormOf TypeField
	| Arrow Form Form
	| Concrete
	deriving Show

data Equiv = Equiv Form Form
	deriving Show

generate :: Type -> [Equiv]
generate field'
	|null args = []
	|otherwise = (Equiv (FormOf $ TypeField (typeName field) []) $ arrowBuild args) : concat (map generate args)
	where
	field = value field'
	args = typeArguments field
	arrowBuild [] = FormOf field
	arrowBuild (x:xs) = Arrow (FormOf $ value x) $ arrowBuild xs

generateIs :: Type -> Type -> [Equiv]
generateIs thing is = Equiv (FormOf $ value thing) (FormOf $ value is) : generate thing

