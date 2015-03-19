
module Parse.Statement where

import Parse.Core
import Parse.Expression
import Parse.Type
import Parse.Modifier
import Lex
import Location

data StatementForm
	= Declare Modifier Type Name (Maybe Expression)
	| Assign Expression Expression
	| Perform Expression
	| If Expression [Statement] [Statement]
	| While Expression [Statement]
	| Break
	| Return (Maybe Expression)
	deriving Show

type Statement = Locate StatementForm

parseDeclare :: Parse Statement
parseDeclare = do
	varAt <- expect (TSpecial "var") (Message $ "expected `var` to begin variable declaration")
	-- next there's a type
	modifier <- parseModifier
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
			return $ Locate varAt $ Declare modifier (Locate typeAt varType) (Locate nameAt name) (Just expr) 
		Just (Locate _ (TSpecial ";")) -> do
			advance 1 -- skip the `;`
			return $ Locate varAt $ Declare modifier (Locate typeAt varType) (Locate nameAt name) Nothing
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
	body <- parseBody $ Message $ "expected `{` to open after if statement beginning at " ++ displayLocation ifAt
	hasElse <- checkNext (TSpecial "else")
	if hasElse then do
			advance 1 -- skip the 'else'
			elseBody <- parseBody $ Message $ "expected `{` to open after else-statement to if-statement beginning at " ++ displayLocation ifAt
			return $ Locate ifAt $ If condition body elseBody
		else
			return $ Locate ifAt $ If condition body []
	

parseWhile :: Parse Statement
parseWhile = do
	whileAt <- expect (TSpecial "while") $ Message $ "expected keyword `while` to begin while-loop"
	expect (TSpecial "(") $ Message $ "expected `(` to follow `while` keyword"
	condition <- parseExpression
	expect (TSpecial ")") $ Message $ "expected `)` to follow condition in while statement, beginning at " ++ displayLocation whileAt
	body <- parseBody $ Message $ "expected `{` to open after while statement beginning at " ++ displayLocation whileAt
	return $ Locate whileAt $ While condition body

parseBreak :: Parse Statement
parseBreak = do
	breakAt <- expect (TSpecial "break") $ Message $ "expected keyword `break` to begin break-statement"
	expect (TSpecial ";") $ Message $ "expected `;` to follow `break` keyword"
	return $ Locate breakAt Break

parseReturn :: Parse Statement
parseReturn = do
	returnAt <- expect (TSpecial "return") $ Message $ "expected keyword `return` to being return-statement"
	emptyBreak <- checkNext (TSpecial ";")
	if emptyBreak then do
		expect (TSpecial ";") $ Message $ "expected `;` to follow `return`"
		return $ Locate returnAt $ Return Nothing
	else do
		expr <- parseExpression
		expect (TSpecial ";") $ Message $ "expected `;` to follow expression for return at " ++ displayLocation returnAt
		return $ Locate returnAt $ Return $ Just expr

parseStatement :: Parse Statement
parseStatement = do
	nextToken <- peekMaybe
	case nextToken of
		Nothing -> crash $ Message $ "expected statement"
		Just (Locate _ (TSpecial "var")) -> parseDeclare
		Just (Locate _ (TSpecial "if")) -> parseIf
		Just (Locate _ (TSpecial "while")) -> parseWhile
		Just (Locate _ (TSpecial "break")) -> parseBreak
		Just (Locate _ (TSpecial "return")) -> parseReturn
		_ -> parseAssignOrPerform

parseBody :: Message -> Parse [Statement]
parseBody message = do
	openAt <- expect (TSpecial "{") message
	body <- manyUntil (checkNext $ TSpecial "}") parseStatement
	expect (TSpecial "}") $ Message $ "expected `}` to close `{` at " ++ displayLocation openAt
	return body

