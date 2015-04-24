
module Verify.Type where

import Message
import Verify
import Syntax.Type
--import Syntax.Module
import Syntax.Class
import Syntax.Member
--import Syntax.Statement
import Syntax.Expression
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

type Environ a = ([Class], Type, [(Type, Name)], a)

environGetType :: Name -> Environ a -> Verify Type
environGetType n (_, _, vs, _) = case select (\(t, n') -> if value n' == value n then Just t else Nothing) vs of
	Just t -> return t
	Nothing -> Left $ Locate (at n) $ Message $ "Attempt to refer to name `" ++ value n ++ "` which is not in scope"

environSetType :: Type -> Name -> Environ a -> Environ a
environSetType t n (a, b, vs, c) = (a, b, (t,n) : vs, c)

environRemoveType :: Name -> Environ a -> Environ a
environRemoveType n (a, b, vs, c) = (a, b, dropFirst vs, c) where
	dropFirst [] = error "attempt to remove thing that's not defined"
	dropFirst ((t, n') : z)
		|value n == value n' = z
		|otherwise = (t, n') : dropFirst z

environMyClass :: Environ a -> Type
environMyClass (_, c, _, _) = c

environValue :: Environ a -> a
environValue (_, _, _, a) = a

environClasses :: Environ a -> [Class]
environClasses (a, _, _, _) = a

environClassGet :: Name -> Environ a -> Verify Class
environClassGet n e = case select (\c -> if value (className c) == value n then Just c else Nothing) $ environClasses e of
	Just t -> return t
	Nothing -> Left $ Locate (at n) $ Message $ "attempt to refer to classname `" ++ value n ++ "` which is not in scope"

-- the environment, the type of the object to index, the name of the field, returns the type (if it exists)
environFieldGet :: Type -> Name -> Environ a -> Verify Type
environFieldGet (Type name classArgs) field environ = do
	objectType <- environClassGet name environ
	case do
		fieldMember <- select checkMember $ classMembers objectType
		return $ relabelType (zipWith (,) (classGeneric objectType) classArgs) fieldMember
		of
		Just t -> return t
		Nothing -> Left $ Locate (at field) $ Message $ "attempt to access field `" ++ value field ++ "` which doesn't exist in object of class type `" ++ value name ++ "`"
	where
	checkMember m = checkMemberValue (memberValue m)
	checkMemberValue (Field { fieldName = n, fieldType = t })
		|value n == value field = Just t
	checkMemberValue _ = Nothing

typeEqual :: Type -> Type -> Bool
typeEqual (Type n a) (Type n' a') = value n == value n' && length a == length a' && all (uncurry typeEqual) (zipWith (,) a a')

-- object index type, method name, argument types, environ
environMethodGet :: Type -> Name -> [Type] -> Environ a -> Verify Type
environMethodGet (Type objectClass classArgs) method methodArgs environ = do
	classType <- environClassGet objectClass environ
	-- the class type for our instance
	-- now we need to select its methods named `method`
	case select (checkMember classType) $ map memberValue $ classMembers classType of
		Just t -> return t
		Nothing -> Left $ Locate (at method) $ Message $ "Attempt to call non-existent method `" ++ value method ++ "` with arguments of types < ... TODO >"
	
	where
	checkMember classType Method{ methodReturnType = returnType, methodName = name, methodArguments = args }
		|value name == value method && all (uncurry typeEqual) (zipWith (,) methodArgs $ map (fixType classType . fst) args)
			= Just $ fixType classType returnType
	checkMember _ _ = Nothing
	fixType classType t = relabelType (zipWith (,) (classGeneric classType) classArgs) t

-- "this" type, expression to get, environ
environExpressionType :: Expression -> Environ a -> Verify Type
environExpressionType expr env = case value expr of
	Name str -> environGetType (Locate (at expr) str) env
	LiteralInt _ -> return (Type (Locate (at expr) "Int") [])
	LiteralString _ -> return (Type (Locate (at expr) "String") [])
	Call (Locate {value = Dot left name}) args -> do -- method call
		leftType <- environExpressionType left env
		argTypes <- mapM (flip environExpressionType env) args
		environMethodGet leftType name argTypes env -- method access
	Dot left name -> do -- field get
		leftType <- environExpressionType left env
		environFieldGet leftType name env -- field access
	Operator left op right -> do
		leftType <- environExpressionType left env
		rightType <- environExpressionType right env
		verifyOperator op leftType rightType
	Prefix op thing -> do
		thingType <- environExpressionType thing env
		verifyPrefix op thingType
	_ -> undefined

verifyPrefix :: Name -> Type -> Verify Type
verifyPrefix Locate{at=loc, value=op} thing
	|op == "not" = (assert (thing == boolType) $ Locate loc $ Message $ "operator to prefix `not` must be of type `Bool` but is of type `" ++ prettyType thing ++ "`") >> return boolType
	|op == "-" = (assert (thing == intType) $ Locate loc $ Message $ "operator to prefix `-` must be of type `Int` but is of type `" ++ prettyType thing ++ "`") >> return intType
verifyPrefix _ _ = error "cannot handle that prefix operator yet"

verifyOperator :: Name -> Type -> Type -> Verify Type
verifyOperator Locate{at=loc, value=op} left right
	|op `elem` ["+","-","*","/","%"] = assertBoth intType >> return intType
	|op `elem` [">","<",">=","<="] = assertBoth intType >> return boolType
	|op `elem` ["==","~="] = assertSame >> return boolType
	|op == "++" = assertBoth stringType >> return stringType
	|op `elem` ["and","or"] = assertBoth boolType >> return boolType

	where
	assertSame = do
		assert (left == right) $ Locate loc $ Message $ "left and right operands to `" ++ op ++ "` have different types, `" ++ prettyType left ++ "` and `" ++ prettyType right ++ "`"
	assertBoth t = do
		assert (left == t) $ Locate loc $ Message $ "left operand to `" ++ op ++ "` does not have type `" ++ prettyType t ++ "` but instead had type `" ++ prettyType left ++ "`"
		assert (right == t) $ Locate loc $ Message $ "right operand to `" ++ op ++ "` does not have type `" ++ prettyType t ++ "` but instead has type `" ++ prettyType right ++ "`"

verifyOperator _ _ _ = error "cannot handle that operator yet"



intType :: Type
intType = Type (Locate (Special "*") "Int") []
stringType :: Type
stringType = Type (Locate (Special "*") "String") []
boolType :: Type
boolType = Type (Locate (Special "*") "Bool") []
