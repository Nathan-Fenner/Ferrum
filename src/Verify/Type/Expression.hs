
module Verify.Type.Expression where

import Verify.Type.Environ
import Message
import Location
import Verify
import Syntax.Expression
import Syntax.Member(Visibility(Public, Private))
import Syntax.Type

-- "this" type, expression to get, environ
environExpressionType :: Expression -> Environ a -> Verify Type
environExpressionType expr env = case value expr of
	Name str -> environGetType (Locate (at expr) str) env
	LiteralInt _ -> return (Type (Locate (at expr) "Int") [])
	LiteralString _ -> return (Type (Locate (at expr) "String") [])
	Call (Locate {value = Dot left name}) args -> do -- method call
		leftType <- environExpressionType left env
		argTypes <- mapM (flip environExpressionType env) args
		environMethodGet leftType name argTypes (accessVisibility leftType) env -- method access
	Call (Locate {value = New t}) args -> do -- constructor call
		argTypes <- mapM (flip environExpressionType env) args
		environConstructorCheck t argTypes (accessVisibility t) env
		return t
	Dot Locate {value = New t} _ -> bareConstructorMessage t
	Dot left name -> do -- field get
		leftType <- environExpressionType left env
		environFieldGet leftType name (accessVisibility leftType) env -- field access
	Operator Locate {value = New t} _ _ -> bareConstructorMessage t
	Operator _ _ Locate {value = New t} -> bareConstructorMessage t
	Operator left op right -> do
		leftType <- environExpressionType left env
		rightType <- environExpressionType right env
		verifyOperator op leftType rightType
	Prefix _ Locate {value = New t} -> bareConstructorMessage t
	Prefix op thing -> do
		thingType <- environExpressionType thing env
		verifyPrefix op thingType
	Index Locate {value = New t} _ -> bareConstructorMessage t
	Index _ Locate {value = New t} -> bareConstructorMessage t
	Index array index -> do
		arrayType <- environExpressionType array env
		indexType <- environExpressionType index env
		assert (value (typeName arrayType) == "Array") $ Locate (at array) $ Message $ "An indexed value must be of type `Array[?]`, but this value is of type `" ++ prettyType arrayType ++ "`"
		assert (indexType == intType) $ Locate (at array) $ Message $ "An indexed value must be of type `Int`, but this value is of type `" ++ prettyType indexType ++ "`"
		-- return array contents:
		return $ head $ typeArguments arrayType
	New t -> bareConstructorMessage t
	where
	accessVisibility t = (if (value $ typeName $ myClass env) == (value $ typeName t) then Private else Public)
	bareConstructorMessage t = Left $ Locate (at $ typeName t) $ Message $ "Constructors require arguments"

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
