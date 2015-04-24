
module Verify.Type where

import Message
import Verify
import Syntax.Type
import Syntax.Module
import Syntax.Class
import Syntax.Member
import Syntax.Statement
import Verify.Type.Expression
import Verify.Type.Environ
import Location




environStatement :: Statement -> Environ a -> Verify (Environ a)
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
	go Break = return env
	go Return{returnExpression = Nothing} = do
		assert (myReturn env == voidType ) $ Locate loc $ Message $ "void methods cannot return values"
		return env
	go Return{returnExpression = Just e} = do
		eType <- environExpressionType e env
		assert (myReturn env == eType) $ Locate loc $ Message $ "method expected to return `" ++ prettyType (myReturn env) ++ "` but returned expression of type `" ++ prettyType eType ++ "` instead"
		return env

environBlock :: [Statement] -> Environ a -> Verify (Environ a)
environBlock [] env = return env
environBlock (s : ss) env = do
	env' <- environStatement s env
	environBlock ss env'

-- it doesn't produce any new meaningful stuffs
-- so just operate on the member!
environMember :: Member -> Environ a -> Verify ()
environMember Member { memberValue = member } env = go member where
	go Method { methodReturnType = returnType, methodArguments = args, methodBody = body } = do
		let env' = env { myReturn = returnType, scope = args ++ scope env }
		_ <- environBlock body env'
		return ()
	go Constructor { constructorArguments = args, constructorBody = body } = do
		let env' = env { myReturn = voidType, scope = args ++ scope env }
		_ <- environBlock body env'
		return ()
	go Field {} = return () -- nothing to check here

environClass :: Class -> Environ a -> Verify ()
environClass Class { className = name, classGeneric = generic, classMembers = members } env = do
	let myType = Type name (map (flip Type []) generic)
	let env' = env { myClass = myType, scope = (myType , Locate (Special "*") "this" ) : scope env }
	mapM_ (flip environMember env') members
	return ()

defaultEnviron :: Module -> Environ ()
defaultEnviron Module { modClasses = classes } = Environ
	{ environClasses = classes
	, myClass = voidType
	, myReturn = voidType
	, scope = []
	, environValue = ()
	}

checkModule :: Module -> Verify ()
checkModule m@Module {modClasses = classes} = let env = defaultEnviron m in mapM_ (flip environClass env) classes
