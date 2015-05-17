
module Verify.Type.Statement where

import Message
import Location
import Verify
import Verify.Type.Environ
import Verify.Type.Expression
import Syntax.Type
import Syntax.Statement


environStatement :: Statement -> Environ -> Verify Environ
environStatement Locate{at=loc, value=statement} env = go statement where
	go Declare { declarationType = varType, declarationName = varName, declarationExpression = expr} = do
		let env' = environSetType varType varName env -- done this way, they shadow without being checked
		case expr of
			Nothing -> return ()
			Just e -> do
				eType <- environExpressionType e env'
				assert (eType == varType) $ Locate (at e) $ Message $ "While declaring variable `" ++ value varName ++ "`, expression given has wrong type; expected `" ++ prettyType varType ++ "` but assigned `" ++ prettyType eType ++ "`"
		return env'
	go Assign { assignLeft = leftExpression, assignRight = rightExpression } = do
		leftType <- environExpressionType leftExpression env
		rightType <- environExpressionType rightExpression env
		assert (leftType == rightType) $ Locate loc $ Message $ "Assignment types do not match; left has type `" ++ prettyType leftType ++ "` while right has type `" ++ prettyType rightType ++ "`"
		return env
	go Perform { performExpression = expression } = do -- just check expression typechecks
		_ <- environExpressionType expression env
		return env
	go If { ifCondition = cond, ifThenBody = thenBody, ifElseBody = elseBody } = do
		condType <- environExpressionType cond env
		assert (condType == boolType) $ Locate loc $ Message $ "if statement must have condition of type `Bool` but is given condition of type `" ++ prettyType condType ++ "` instead"
		_ <- environBlock thenBody env
		_ <- environBlock elseBody env
		return env
	go While { whileCondition = cond, whileBody = body } = do
		condType <- environExpressionType cond env
		assert (condType == boolType) $ Locate loc $ Message $ "while statement must have condition of type `Bool` but is given condition of type `" ++ prettyType condType ++ "` instead"
		_ <- environBlock body env
		return env
	go Forever { foreverBody = body } = environBlock body env
	go Break = return env
	go Return{returnExpression = Nothing} = do
		assert (myReturn env == voidType ) $ Locate loc $ Message $ "void methods cannot return values"
		return env
	go Return{returnExpression = Just e} = do
		eType <- environExpressionType e env
		assert (myReturn env == eType) $ Locate loc $ Message $ "method expected to return `" ++ prettyType (myReturn env) ++ "` but returned expression of type `" ++ prettyType eType ++ "` instead"
		return env

environBlock :: [Statement] -> Environ -> Verify (Environ)
environBlock [] env = return env
environBlock (s : ss) env = do
	env' <- environStatement s env
	environBlock ss env'