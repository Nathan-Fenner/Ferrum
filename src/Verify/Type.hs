
module Verify.Type where

import Syntax.Type
import Location

relabelType :: String -> String -> Type -> Type
relabelType from to given = Type
		(if from == value givenName then newName else givenName)
		(map (relabelType from to) $ typeArguments given)
	where
	newName = Locate (at givenName) to
	givenName = typeName $ given
