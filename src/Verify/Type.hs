
module Verify.Type where

import Verify
import Syntax.Type
import Syntax.Module
import Syntax.Class
import Syntax.Member
import Syntax.Statement
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
typeCheckClass classes c = mapM_ (typeCheckMember classes c) (classMembers c)

typeCheckMember :: [Class] -> Class -> Member -> Verify ()
typeCheckMember classes mine Member{memberVisibility = visibility, memberValue = member} = typeCheckMemberValue classes mine visibility member

typeCheckMemberValue :: [Class] -> Class -> Visibility -> MemberValue -> Verify ()
typeCheckMemberValue classes mine visibility (Method
		{ methodReturnType = returnType
		, methodName = name
		, methodArguments = arguments
		, methodBody = body
		}) = typeCheckBody classes mine visibility returnType name (thisType : arguments) body
		where
		generics = classGeneric mine
		mineType = Type (className mine) $ map (flip Type []) generics
		thisType = (mineType, Locate (Special "this") "this")

typeCheckMemberValue _classes _mine _visibility _ = return ()
-- fields need no checking yet, since they cannot be initialized

typeCheckBody :: [Class] -> Class -> Visibility -> Type -> Name -> [(Type, Name)] -> [Statement] -> Verify ()
typeCheckBody = undefined



