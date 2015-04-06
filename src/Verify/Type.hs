
module Verify.Type where

import Message
import Verify
import Syntax.Type
import Syntax.Module
import Syntax.Class
import Syntax.Member
import Syntax.Statement
import Syntax.Expression
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
typeCheckMember classes mine Member{memberValue = member} = typeCheckMemberValue classes mine member

typeCheckMemberValue :: [Class] -> Class -> MemberValue -> Verify ()
typeCheckMemberValue classes mine (Method
		{ methodReturnType = returnType
		, methodName = name
		, methodArguments = arguments
		, methodBody = body
		}) = typeCheckBody classes mine returnType name (thisType : arguments) body
		where
		generics = classGeneric mine
		mineType = Type (className mine) $ map (flip Type []) generics
		thisType = (mineType, Locate (Special "this") "this")

typeCheckMemberValue _classes _mine _ = return ()
-- fields need no checking yet, since they cannot be initialized

typeCheckBody :: [Class] -> Class -> Type -> Name -> [(Type, Name)] -> [Statement] -> Verify ()
-- if there are no statements, we require the returnType given is Void
typeCheckBody _classes _mine (Type (Locate _ "Void") []) _name _scope [] = return ()
typeCheckBody _classes mine returnType name _scope [] = Left $ Locate (at name) $ Message $ "Method `" ++ value name ++ "` of class `" ++ (value . className $ mine) ++ "` must return type " ++ show returnType ++ " but reaches the end of its body without a return."
typeCheckBody _classes mine _ name _scope (Locate loc (Return _) : _ : _) = Left $ Locate loc $ Message $ "code continues after explicit `return` in method `" ++ value name ++ " defined at " ++ displayLocation (at name) ++ " in class `" ++ (value . className $ mine) ++ "`"
typeCheckBody _classes _mine (Type (Locate _ "Void") []) _name _scope [Locate _ (Return Nothing)] = return ()
typeCheckBody _classes mine (Type (Locate _ "Void") []) name _scope [Locate _ (Return (Just r))] = Left $ Locate (at r) $ Message $ "method `" ++ value name ++ "` in class `" ++ (value . className $ mine) ++ "` is of type Void but returns a value"

typeCheckBody _ _ _ _ _ _ = undefined

typeCheckExpression :: [Class] -> [(Type, Name)] -> Expression -> Verify Type
typeCheckExpression = undefined

