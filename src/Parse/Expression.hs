
module Parse.Expression where

import Parse.Core
import Lex
import Location

type Expression = Locate ExpressionValue

data ExpressionValue
	= Name String
	| LiteralInt Int
	| LiteralString String
	deriving Show


parseAtom :: Parse Expression
parseAtom = do
	Locate at token <- ask message
	case token of
		TWord name -> return $ Locate at $ Name name
		TInt int -> return $ Locate at $ LiteralInt int
		TString string -> return $ Locate at $ LiteralString string
		_ -> crash message
	where
		message = Message "expected literal"
