
module Verify.Type where

import Verify
import Syntax.Type
import Syntax.Module
import Verify.Type.Environ
import Verify.Type.Class

defaultEnviron :: Module -> Environ ()
defaultEnviron Module { modClasses = classes } = Environ
	{ environClasses = classes
	, myClass = voidType
	, myReturn = voidType
	, scope = []
	, environValue = ()
	}

checkModule :: Module -> Verify ()
checkModule m@Module {modClasses = classes} = let env = defaultEnviron m in mapM_ (flip environClass env) classes
