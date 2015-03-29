
module Verify.Type where
import Parse.Core
import Parse.Type
import Parse.Statement
import Parse.Member
import Parse.Class
import Parse.Module
import Verify
import Verify.Type.Form
import Location

relabelType :: String -> String -> Type -> Type
relabelType from to given = Locate (at givenName) $
	TypeField
		(if from == value givenName then newName else givenName)
		(map (relabelType from to) $ typeArguments typeField)
	where
	typeField = value given
	newName = Locate (at givenName) to
	givenName = typeName $ typeField

relabelForm :: String -> String -> Form -> Form
relabelForm from to (FormOf t) = FormOf $ relabelType from to t
relabelForm from to (Arrow left right) = Arrow (relabelForm from to left) (relabelForm from to right)
relabelForm from to c = c

relabelEquiv :: String -> String -> Equiv -> Equiv
relabelEquiv from to (Equiv equivType equivForm) = Equiv
	(relabelType from to equivType)
	(relabelForm from to equivForm)

extractFormsModule :: Module -> [Equiv]
extractFormsModule m = concat $ map extractFormsClass $ modClasses m

extractFormsClass :: Class -> [Equiv]
extractFormsClass c = undefined
	where
	generics = classGeneric c
	members = classMembers c
	name = className c

