
module Verify.Type where

import Verify
import Syntax.Type
import Syntax.Module
import Syntax.Class
import Syntax.Member
import Location

relabelType :: String -> String -> Type -> Type
relabelType from to given = Type
		(if from == value givenName then newName else givenName)
		(map (relabelType from to) $ typeArguments given)
	where
	newName = Locate (at givenName) to
	givenName = typeName $ given

typeCheckModule :: Module -> Verify ()
typeCheckModule m = mapM_ (typeCheckClass $ modClasses m) $ modClasses m

typeCheckClass :: [Class] -> Class -> Verify ()
typeCheckClass classes c = mapM_ (typeCheckMember classes) (classMembers c)

typeCheckMember :: [Class] -> Member -> Verify ()
typeCheckMember = undefined

