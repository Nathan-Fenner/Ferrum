
module Parse.Expression where

import Parse.Core
import Lex
import Location

type Expression = Locate ExpressionValue

data ExpressionValue
	= Name String
	| LiteralInt Int
	deriving Show

parseName :: Parse Expression
parseName = do
	Locate at token <- ask message
	case token of
		TWord name -> do
			return $ Locate at $ Name name
		_ -> crash message

	where
		message = Message "expected name"

parseInt :: Parse Expression
parseInt = do
	Locate at token <- ask message
	case token of
		TInt int -> return $ Locate at $ LiteralInt int
		_ -> crash message
	where
		message = Message "expected literal integer"