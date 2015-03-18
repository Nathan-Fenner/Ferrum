
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
	expect (TSpecial ";") $ Message $ "expected semicolon to follow declaration of variable `" ++ name ++ "` at " ++ displayLocation nameAt ++ " (declaration begins at " ++ displayLocation varAt ++ ")"
	return $ Locate varAt $ Declare (Locate typeAt varType) (Locate nameAt name) Nothing


