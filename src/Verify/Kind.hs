
module Verify.Kind where

import Syntax.Kind
import Syntax.Class

kindOfClass :: Class -> Kind
kindOfClass c = case classKind c of
	Nothing -> simpleKind (length classGeneric)
	Just k -> k

