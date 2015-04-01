
module Syntax.Modifier where

data Modifier
	= None
	| Mutable
	| Alterable
	| Final
	deriving (Show, Eq)

