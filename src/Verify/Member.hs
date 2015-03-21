module Verify.Member where

import Verify
import Verify.Type
import Parse.Member
import Parse.Type
import Location

concrete :: Type -> Verify [ArityFact]
concrete given = let c = value given in  return [makeArity (typeName c) (length $ typeArguments c)]

verifyMemberGenericArity :: Member -> Verify [ArityFact]
verifyMemberGenericArity member = case memberValue member of
	field @ Field {} -> concrete $ fieldType field
	method @ Method {} -> unifyVerifyArity $ map concrete $ methodReturnType method : map fst (methodArguments method)
	constructor @ Constructor {} -> unifyVerifyArity $ map (concrete . fst) $ constructorArguments constructor