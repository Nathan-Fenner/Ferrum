module Verify.Kind.Statement where

import Verify
import Verify.Kind.Type
import Location
import Syntax.Kind
import Syntax.Statement

verifyStatementKind :: [(String, Kind)] -> Statement -> Verify ()
verifyStatementKind known statement' = case statement of
	Declare { declarationType = declared } -> verifyTypeConcrete known declared
	If { ifThenBody = thenBody, ifElseBody = elseBody } -> do
		verifyBlockKind known thenBody
		verifyBlockKind known elseBody
	While { whileBody = body } -> verifyBlockKind known body
	_ -> return ()
	where
	statement = value statement'

verifyBlockKind :: [(String,Kind)] -> [Statement] -> Verify ()
verifyBlockKind known = mapM_ (verifyStatementKind known)