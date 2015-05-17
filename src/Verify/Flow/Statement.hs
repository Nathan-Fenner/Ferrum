
module Verify.Flow.Statement where

import Verify
import Message
import Location
import Syntax.Statement

data FlowStatus = FlowStatus
	{ mayProceed :: Bool
	, mayReturn :: Bool
	, mayBreak :: Bool
	} deriving Show

mergeEitherState :: FlowStatus -> FlowStatus -> FlowStatus
mergeEitherState (FlowStatus a b c) (FlowStatus x y z) = FlowStatus (a || x) (b || y) (c || z)

mergeSeqState :: FlowStatus -> FlowStatus -> FlowStatus
mergeSeqState (FlowStatus a b _) (FlowStatus x y z) = FlowStatus (a && x) (b && y) z

lineFlowStatus :: Statement -> Verify FlowStatus
lineFlowStatus Locate { value = Break } = return $ FlowStatus { mayProceed = False, mayReturn = False, mayBreak = True }
lineFlowStatus Locate { value = Return {} } = return $ FlowStatus { mayProceed = False, mayReturn = True, mayBreak = False }
lineFlowStatus Locate { at = here, value = While { whileBody = body } } = do
	bodyFlow <- blockFlowStatus body
	if not (mayProceed bodyFlow)
		then Left $ Locate here $ Message $ "the end of the while loop is unreachable"
		else return $ FlowStatus { mayProceed = True, mayReturn = mayReturn bodyFlow, mayBreak = False }
lineFlowStatus Locate { value = If { ifThenBody = thenBody, ifElseBody = elseBody } } = do
	thenFlow <- blockFlowStatus thenBody
	elseFlow <- blockFlowStatus elseBody
	return $ mergeEitherState thenFlow elseFlow
lineFlowStatus _ = return $ FlowStatus { mayProceed = True, mayReturn = False, mayBreak = False }

unreachableAfter :: FlowStatus -> Bool
unreachableAfter = not . mayProceed

blockFlowStatus' :: FlowStatus -> [Statement] -> Verify FlowStatus
blockFlowStatus' state [] = return state
blockFlowStatus' state (first:rest)
	|unreachableAfter state = Left $ Locate (at first) $ Message $ "statement is unreachable"
	|otherwise = do
		firstState <- lineFlowStatus first
		blockFlowStatus' (mergeSeqState state firstState) rest

-- this is the one that's principally exposed
blockFlowStatus :: [Statement] -> Verify FlowStatus
blockFlowStatus code = blockFlowStatus' (FlowStatus { mayProceed = True, mayReturn = False, mayBreak = False }) code

-- code, isVoid
methodBodyFlowVerify :: [Statement] -> Bool -> Verify ()
methodBodyFlowVerify code isVoid = do
	status <- blockFlowStatus code
	if mayBreak status then Left $ Locate (Special "*") $ Message $ "methods cannot have a top-level break"
		else return ()
	if not isVoid && mayProceed status then Left $ Locate (Special "*") $ Message $ "non-void methods must return a value"
		else return ()
	return ()

