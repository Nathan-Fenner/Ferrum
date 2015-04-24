
module Verify.Type where

import Verify
import Syntax.Type
import Syntax.Module
import Syntax.Class
import Syntax.Member
import Verify.Type.Environ
import Verify.Type.Statement
import Verify.Type.Member
import Location



environClass :: Class -> Environ a -> Verify ()
environClass Class { className = name, classGeneric = generic, classMembers = members } env = do
	let myType = Type name (map (flip Type []) generic)
	let env' = env { myClass = myType, scope = (myType , Locate (Special "*") "this" ) : scope env }
	mapM_ (flip environMember env') members
	return ()

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
