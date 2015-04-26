
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
		environMethodGet leftType name argTypes env -- method access
	Dot left name -> do -- field get
		leftType <- environExpressionType left env
		environFieldGet leftType name (if (value $ typeName $ myClass env) == (value $ typeName leftType) then Private else Public) env -- field access
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
