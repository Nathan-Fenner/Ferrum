
module Parse.Expression where

import Parse.Core
import Lex
import Location

type Expression = Locate ExpressionValue

data ExpressionValue = Reference String

parseExpression :: Parse (Locate Token) Message Expression
parseExpression = do
	Locate at token <- ask message
	case token of
		TWord name -> do
			advance 1 -- consume the name
			return $ Locate at $ Reference name
		_ -> crash message

	where
		message = Message "expected name"

