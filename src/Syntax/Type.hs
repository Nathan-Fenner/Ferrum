
module Syntax.Type where

import Location

data Type
	= Type { typeName :: Name, typeArguments :: [Type] }
	deriving Show

typeAt :: Type -> Location
typeAt = at . typeName
