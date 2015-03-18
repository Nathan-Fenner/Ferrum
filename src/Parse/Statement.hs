
module Parse.Statement where

import Parse.Core
import Parse.Expression
import Location

data StatementForm
	= Declare (Locate String) (Maybe Expression)
	| Assign Expression Expression
	| Perform Expression
	| If Expression [Statement] [Statement]
	| While Expression [Statement]
	| Break
	| Return (Maybe Statement)
	deriving Show

type Statement = Locate StatementForm

