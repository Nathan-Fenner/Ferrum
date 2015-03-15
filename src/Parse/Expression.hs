
module Parse.Expression where

import Parse.Core
import Lex
import Location

type Expression = Locate ExpressionValue

data ExpressionValue = Reference String