
module Verify.Kind.Member where

import Verify
import Verify.Kind.Type
import Verify.Kind.Statement
import Syntax.Member
import Syntax.Kind

verifyMemberKind :: [(String, Kind)] -> Member -> Verify ()
verifyMemberKind known member = case memberValue member of
	Field { fieldType = declared } -> verifyTypeConcrete known declared
	Method { methodReturnType = returnType, methodArguments = arguments, methodBody = body } -> do
		verifyTypeConcrete known returnType
		mapM_ (verifyTypeConcrete known . fst) arguments
		verifyBlockKind known body
	Constructor { constructorArguments = arguments, constructorBody = body } -> do
		mapM_ (verifyTypeConcrete known . fst) arguments
		verifyBlockKind known body