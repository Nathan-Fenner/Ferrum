
module Syntax.Expression where

import Location

type Expression = Locate ExpressionValue

data ExpressionValue
	= Name String
	| LiteralInt Int
	| LiteralString String
	| Operator Expression Name Expression
	| Prefix Name Expression
	| Call Expression [Expression]
	| Index Expression Expression
	| Dot Expression Name
	deriving Show

