
module Verify.Type.Environ where

import Syntax.Type
import Syntax.Class
import Syntax.Member
import Message
import Location
import Verify

(#>>#) :: Monad m => (Environ -> m b) -> (Environ -> m c) -> Environ -> m c
(f #>># g) env = f env >> g env

(#>>=#) :: Monad m => (Environ -> m b) -> (b -> Environ -> m c) -> Environ -> m c
(f #>>=# g) env = f env >>= flip g env

data Environ = Environ
	{ environClasses :: [Class]
	, myClass :: Type
	, myReturn :: Type
	, scope :: [(Type, Name)]
	}

environGetType :: Name -> Environ -> Verify Type
environGetType n Environ{scope = vs} = case select (\(t, n') -> if value n' == value n then Just t else Nothing) vs of
	Just t -> return t
	Nothing -> Left $ Locate (at n) $ Message $ "Attempt to refer to name `" ++ value n ++ "` which is not in scope"

environSetType :: Type -> Name -> Environ -> Environ
environSetType t n e@Environ{scope=vs} = e {scope=(t,n) : vs}

environRemoveType :: Name -> Environ -> Environ
environRemoveType n e@Environ{scope=vs} = e {scope = dropFirst vs} where
	dropFirst [] = error "attempt to remove thing that's not defined"
	dropFirst ((t, n') : z)
		|value n == value n' = z
		|otherwise = (t, n') : dropFirst z



environClassGet :: Name -> Environ -> Verify Class
environClassGet n e = case select (\c -> if value (className c) == value n then Just c else Nothing) $ environClasses e of
	Just t -> return t
	Nothing -> Left $ Locate (at n) $ Message $ "attempt to refer to classname `" ++ value n ++ "` which is not in scope"

-- the environment, the type of the object to index, the name of the field, returns the type (if it exists)
environFieldGet :: Type -> Name -> Visibility -> Environ -> Verify Type
environFieldGet accessType@(Type name classArgs) field visibility environ = do
	objectType <- environClassGet name environ
	case do
		fieldMember <- select checkMember $ classMembers objectType
		return $ relabelType (zipWith (,) (classGeneric objectType) classArgs) fieldMember
		of
		Just t -> return t
		Nothing -> Left $ Locate (at field) $ Message $ "attempt to access field `" ++ value field ++ "` which doesn't exist " ++ (if visibility == Public then "or isn't public " else "") ++ "in object of type `" ++ prettyType accessType ++ "`"
	where
	checkMember :: Member -> Maybe Type
	checkMember m
		|memberVisibility m > visibility = Nothing -- not accessible
		|otherwise = checkMemberValue (memberValue m)
	checkMemberValue (Field { fieldName = n, fieldType = t })
		|value n == value field = Just t
	checkMemberValue _ = Nothing

-- object index type, method name, argument types, environ
environMethodGet :: Type -> Name -> [Type] -> Visibility -> Environ -> Verify Type
environMethodGet (Type objectClass classArgs) method methodArgs visibility environ = do
	classType <- environClassGet objectClass environ
	-- the class type for our instance
	-- now we need to select its methods named `method`
	case select (checkMember classType) $ classMembers classType of
		Just t -> return t
		Nothing -> Left $ Locate (at method) $ Message $ "Attempt to call non-existent " ++ (if visibility == Public then "or private " else "") ++ "method `" ++ value method ++ "` with arguments of types < ... TODO >"
	where
	checkMember :: Class -> Member -> Maybe Type
	checkMember c m
		|memberVisibility m > visibility = Nothing
		|otherwise = checkMemberValue c (memberValue m)
	checkMemberValue :: Class -> MemberValue -> Maybe Type
	checkMemberValue classType Method{ methodReturnType = returnType, methodName = name, methodArguments = args }
		|value name == value method && all (uncurry (==)) (zipWith (,) methodArgs $ map (fixType classType . fst) args)
			= Just $ fixType classType returnType
	checkMemberValue _ _ = Nothing
	fixType classType t = relabelType (zipWith (,) (classGeneric classType) classArgs) t

environConstructorCheck :: Type -> [Type] -> Visibility -> Environ -> Verify ()
environConstructorCheck (Type objectClass classArgs) conArgs visibility environ = do
	classType <- environClassGet objectClass environ
	case select (checkMember classType) $ classMembers classType of
		Just _ -> return ()
		Nothing -> Left $ Locate (at objectClass) $ Message $ "Constructor has no applicable (public) constructor taking arguments of type `(" ++ (concat $ map (\t -> prettyType t ++ ", ") conArgs) ++ ")`"
	where
	checkMember :: Class -> Member -> Maybe ()
	checkMember c m
		|memberVisibility m > visibility = Nothing
		|otherwise = checkMemberValue c (memberValue m)
	checkMemberValue :: Class -> MemberValue -> Maybe ()
	checkMemberValue classType Constructor{ constructorArguments = args }
		|all (uncurry (==)) (zipWith (,) conArgs $ map (fixType classType . fst) args)
			= Just ()
	checkMemberValue _ _ = Nothing
	fixType classType t = relabelType (zipWith (,) (classGeneric classType) classArgs) t
