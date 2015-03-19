
module Parse.Statement where

import Parse.Core
import Parse.Expression
import Parse.Type
import Lex
import Location

data StatementForm
	= Declare Type Name (Maybe Expression)
	| Assign Expression Expression
	| Perform Expression
	| If Expression [Statement] [Statement]
	| While Expression [Statement]
	| Break
	| Return (Maybe Statement)
	deriving Show

type Statement = Locate StatementForm

parseDeclare :: Parse Statement
parseDeclare = do
	varAt <- expect (TSpecial "var") (Message $ "expected `var` to begin variable declaration")
	-- next there's a type
	Locate typeAt varType <- parseType
	expect (TSpecial ":") (Message $ "expected `:` to follow variable declaration type starting at " ++ displayLocation typeAt)
	Locate nameAt name <- expectName $ Message $ "expected name to follow `:` in type declaration beginning at " ++ displayLocation varAt
	-- here, we may see an `=` instead of a `;`
	next <- peekMaybe
	case next of
		Just (Locate _ (TOperator "=")) -> do
			advance 1 -- skip the `=`
			-- now get an expression
			expr <- parseExpression
			expect (TSpecial ";") $ Message $ "expected an `;` to follow declaration assignment of `" ++ name ++ "` at " ++ displayLocation nameAt
			return $ Locate varAt $ Declare (Locate typeAt varType) (Locate nameAt name) (Just expr) 
		Just (Locate _ (TSpecial ";")) -> do
			advance 1 -- skip the `;`
			return $ Locate varAt $ Declare (Locate typeAt varType) (Locate nameAt name) Nothing
		_ -> crash $ Message $ "expected an `=` or a `;` to follow variable declaration of `" ++ name ++ "` beginning at " ++ displayLocation varAt



parseAssignOrPerform :: Parse Statement
parseAssignOrPerform = do
	Locate exprAt expr <- parseExpression -- the left
	-- case on the next symbol:
	-- `;` means Perform
	-- `=` means Assign
	-- anything else is an error
	next <- peekMaybe
	case next of
		Just (Locate _ (TSpecial ";")) -> do
			advance 1 -- skip the `;`
			return $ Locate exprAt (Perform (Locate exprAt expr))
		Just (Locate eqAt (TOperator "=")) -> do
			advance 1 -- skip the `=`
			Locate rightAt right <- parseExpression
			expect (TSpecial ";") $ Message $ "Expected `;` to follow assignment at " ++ displayLocation eqAt
			return $ Locate exprAt $ Assign (Locate exprAt expr) (Locate rightAt right)

parseIf :: Parse Statement
parseIf = do
	ifAt <- expect (TSpecial "if") $ Message $ "expected keyword `if` to begin if-statement"
	expect (TSpecial "(") $ Message $ "expected `(` to follow `if` keyword"
	condition <- parseExpression
	expect (TSpecial ")") $ Message $ "expected `)` to follow condition in if statement, beginning at " ++ displayLocation ifAt
	openAt <- expect (TSpecial "{") $ Message $ "expected `{` to open after if statement beginning at " ++ displayLocation ifAt
	body <- manyUntil (checkNext $ TSpecial "}") parseStatement
	expect (TSpecial "}") $ Message $ "expected `}` to close after `{` that opened at " ++ displayLocation openAt
	return $ Locate ifAt $ If condition body []

parseStatement :: Parse Statement
parseStatement = undefined
