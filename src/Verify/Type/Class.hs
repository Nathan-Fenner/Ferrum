
module Verify.Type.Class where

import Location
import Verify
import Verify.Type.Environ
import Verify.Type.Member
import Syntax.Type
import Syntax.Class

environClass :: Class -> Environ a -> Verify ()
environClass Class { className = name, classGeneric = generic, classMembers = members } env = do
	let myType = Type name (map (flip Type []) generic)
	let env' = env { myClass = myType, scope = (myType , Locate (Special "*") "this" ) : scope env }
	mapM_ (flip environMember env') members
	return ()