
module Syntax.Effect where

import Location
import Syntax.Expression

data EffectValue
	= Modify Expression
	| Alter Expression
	| Retain Expression Expression
	| Effect Expression
	deriving Show

type Effect = Locate EffectValue
