
module Verify.Type.Member where

import Verify
import Verify.Type.Environ
import Verify.Type.Statement
import Syntax.Type
import Syntax.Member

-- it doesn't produce any new meaningful stuffs
-- so just operate on the member!
environMember :: Member -> Environ -> Verify ()
environMember Member { memberValue = member } env = go member where
	go Method { methodReturnType = returnType, methodArguments = args, methodBody = body } = do
		let env' = env { myReturn = returnType, scope = args ++ scope env }
		_ <- environBlock body env'
		return ()
	go Constructor { constructorArguments = args, constructorBody = body } = do
		let env' = env { myReturn = voidType, scope = args ++ scope env }
		_ <- environBlock body env'
		return ()
	go Field {} = return () -- nothing to check here
