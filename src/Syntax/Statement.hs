
module Syntax.Statement where
import Syntax.Expression
import Syntax.Modifier
import Syntax.Type
import Location

data StatementForm
	= Declare
		{ declareModifier :: Modifier
		, declarationType :: Type
		, declarationName :: Name
		, declarationExpression :: (Maybe Expression)
		}
	| Assign
		{ assignLeft :: Expression
		, assignRight :: Expression
		}
	| Perform
		{ performExpression :: Expression
		}
	| If
		{ ifCondition :: Expression
		, ifThenBody :: [Statement]
		, ifElseBody :: [Statement]
		}
	| While
		{ whileCondition :: Expression
		, whileBody :: [Statement]
		}
	| Break
	| Return
		{ returnExpression :: (Maybe Expression)
		}
	deriving Show

type Statement = Locate StatementForm
