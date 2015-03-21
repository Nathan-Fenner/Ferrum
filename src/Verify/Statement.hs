
module Verify.Statement where
import Verify
import Verify.Type
import Parse.Statement
import Location

verifyStatementGenericArity :: Statement -> Verify [ArityFact]
verifyStatementGenericArity statement = case value statement of
	declarer@Declare {} -> concrete $ declarationType declarer
	ifStatement@If {} -> unifyVerifyArity $
		map verifyStatementGenericArity (ifThenBody ifStatement) ++ map verifyStatementGenericArity (ifElseBody ifStatement)
	whileStatement@While {} -> unifyVerifyArity $
		map verifyStatementGenericArity (whileBody whileStatement)
	_ -> return []
{-
StatementForm = Declare | Assign | Perform | If | While | Break | Return
-}