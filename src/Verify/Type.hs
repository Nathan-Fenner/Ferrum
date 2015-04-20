
module Verify.Type where

--import Message
--import Verify
import Syntax.Type
--import Syntax.Module
import Syntax.Class
import Syntax.Member
--import Syntax.Statement
--import Syntax.Expression
import Location

select :: (a -> Maybe b) -> [a] -> Maybe b
select f l = go $ map f l where
	go [] = Nothing
	go (Just x : _) = Just x
	go (_ : xs) = go xs

-- generics are given
relabelType :: [(Name, Type)] -> Type -> Type
relabelType generics (Type name args) = lookUp name `applyArguments` map (relabelType generics) args
	where
	lookUp :: Name -> Type
	lookUp n = case select (\(match, result) -> if value match == value n then Just result else Nothing) generics of
		Nothing -> Type n []
		Just r -> r
	applyArguments :: Type -> [Type] -> Type
	applyArguments (Type n a) more = Type n (a ++ more)

type Environ a = ([Class], Class, [(Type, Name)], a)
environValue :: Environ a -> a
environValue (_, _, _, a) = a

environClasses :: Environ a -> [Class]
environClasses (a, _, _, _) = a

environClassGet :: String -> Environ a -> Maybe Class
environClassGet n e = select (\c -> if value (className c) == n then Just c else Nothing) $ environClasses e

-- the environment, the type of the object to index, the name of the field, returns the type (if it exists)
environFieldGet :: Type -> String -> Environ a -> Maybe Type
environFieldGet (Type name classArgs) field environ = do
	objectType <- environClassGet (value name) environ
	fieldMember <- select checkMember $ classMembers objectType
	return $ relabelType (zipWith (,) (classGeneric objectType) classArgs) fieldMember
	where
	checkMember m = checkMemberValue (memberValue m)
	checkMemberValue (Field { fieldName = n, fieldType = t })
		|value n == field = Just t
	checkMemberValue _ = Nothing
