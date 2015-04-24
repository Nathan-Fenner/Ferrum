
module Verify.Type.Module(checkModule) where

import Verify
import Verify.Type.Environ
import Verify.Type.Class
import Syntax.Type
import Syntax.Module

checkModule :: Module -> Verify ()
checkModule m@Module {modClasses = classes} = let env = defaultEnviron m in mapM_ (flip environClass env) classes

defaultEnviron :: Module -> Environ ()
defaultEnviron Module { modClasses = classes } = Environ
	{ environClasses = classes
	, myClass = voidType
	, myReturn = voidType
	, scope = []
	, environValue = ()
	}

