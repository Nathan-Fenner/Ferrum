
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

relabelType :: [(Name, Name)] -> Type -> Type
relabelType rules (Type name args) = case select (\(l,_) -> value l == value name) rules of
	Nothing -> Type name (map (relabelType rules) args)
	Just (_, r) -> Type r (map (relabelType rules) args)
	where
	select _ [] = Nothing
	select f (a : z)
		|f a = Just a
		|otherwise = select f z

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

typeCheckExpression :: [Class] -> Class -> [(Type, Name)] -> Expression -> Verify Type
typeCheckExpression classes mine scope e = case value e of
	Name name -> case filter (\(_, n) -> value n == name) scope of
		[] -> Left $ Locate loc $ Message $ "no reference to name `" ++ name ++ "`. It may be out-of-scope or misspelled"
		[(t, _)] -> return t
		_ -> error "verification is violating consistency of scope table"
	LiteralInt _ -> return $ Type (Locate loc "Int") []
	LiteralString _ -> return $ Type (Locate loc "String") []
	Dot left name -> do
		Type leftName genericArgs <- typeCheckExpression classes mine scope left
		case filter (\c -> value (className c) == value leftName) classes of
			[] -> Left $ Locate loc $ Message $ "no ability to access member of type `" ++ value leftName ++ "`. This class may be internal and unindexable."
			[c] -> case findFieldType c name (classMembers c) of
				Nothing -> Left $ Locate loc $ Message $ "no access to field `" ++ value name ++ "` of class of type `" ++ value (className c) ++ "`. It may be private or non-existent."
				Just t -> return $ relabelType (zipWith (,) (classGeneric c) (map typeName $ genericArgs) ) t
			_ -> error "verification is violating consistency of class table"
	Call (Locate _ (Dot left name)) args -> undefined
	Call (Locate _ fun) args -> undefined
	where
	findFieldType within name [] = Nothing
	findFieldType within name (m : ms) = case Public == memberVisibility m || value (className within) == value (className mine) of
		True -> case memberValue m of
			Field { fieldName = n, fieldType = t } -> if value n == value name then Just t else findFieldType within name ms
		False -> findFieldType within name ms
	loc = at e
{-
	| Operator Expression Name Expression
	| Prefix Name Expression
	| Index Expression Expression
-}

