
module Verify.Type where

import Syntax.Type
import Syntax.Class
import Syntax.Module
import Syntax.Member
import Verify.Type.Form
import Location

relabelType :: String -> String -> Type -> Type
relabelType from to given = Type
		(if from == value givenName then newName else givenName)
		(map (relabelType from to) $ typeArguments given)
	where
	newName = Locate (at givenName) to
	givenName = typeName $ given

relabelForm :: String -> String -> Form -> Form
relabelForm from to (FormOf t) = FormOf $ relabelType from to t
relabelForm from to (Arrow left right) = Arrow (relabelForm from to left) (relabelForm from to right)
relabelForm _from _to c = c

relabelEquiv :: String -> String -> Equiv -> Equiv
relabelEquiv from to (Equiv equivType equivForm) = Equiv
	(relabelType from to equivType)
	(relabelForm from to equivForm)

extractFormsModule :: Module -> [Equiv]
extractFormsModule m = concat $ map extractFormsClass $ modClasses m

extractFormsClass :: Class -> [Equiv]
extractFormsClass c = undefined : concat (map extractFormsMember members)
	where
	members = classMembers c
	name = className c

extractFormsMember :: Member -> [Equiv]
extractFormsMember = undefined

