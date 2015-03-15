
module Parse.Expression where

import Parse.Core
import Lex
import Location

type Expression = Locate ExpressionValue

data ExpressionValue
	= Name String
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
