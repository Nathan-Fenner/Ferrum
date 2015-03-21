module Verify.Member where

import Verify
import Verify.Type
import Verify.Statement
import Parse.Member


verifyMemberGenericArity :: Member -> Verify [ArityFact]
verifyMemberGenericArity member = case memberValue member of
	field @ Field {} -> concrete $ fieldType field
	method @ Method {} -> unifyVerifyArity $
		(map verifyStatementGenericArity $ methodBody method)
		++
		(map concrete $ methodReturnType method : map fst (methodArguments method))
	constructor @ Constructor {} -> unifyVerifyArity $
		(map verifyStatementGenericArity $ constructorBody constructor)
		++
		(map (concrete . fst) $ constructorArguments constructor)