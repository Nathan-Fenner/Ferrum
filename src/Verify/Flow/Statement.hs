
module Verify.Flow.Statement where

import Verify
import Message
import Location
import Syntax.Statement

data FlowChoice = Will | Wont | Might deriving (Show, Eq)

data FlowStatus = FlowStatus
	{ returnStatus :: FlowChoice
	, breakStatus :: FlowChoice
	} deriving Show

mergeEitherChoice :: FlowChoice -> FlowChoice -> FlowChoice
mergeEitherChoice Will Will = Will
mergeEitherChoice Wont Wont = Wont
mergeEitherChoice _    _    = Might

mergeSeqChoice :: FlowChoice -> FlowChoice -> FlowChoice
mergeSeqChoice Will _ = Will
mergeSeqChoice Wont x = x
mergeSeqChoice Might Wont = Might
mergeSeqChoice Might x = x

mergeEitherState :: FlowStatus -> FlowStatus -> FlowStatus
mergeEitherState (FlowStatus a b) (FlowStatus x y) = FlowStatus (mergeEitherChoice a x) (mergeEitherChoice b y)

mergeSeqState :: FlowStatus -> FlowStatus -> FlowStatus
mergeSeqState (FlowStatus a b) (FlowStatus x y) = FlowStatus (mergeSeqChoice a x) (mergeSeqChoice b y)

lineFlowStatus :: Statement -> Verify FlowStatus
lineFlowStatus Locate { value = Break } = return $ FlowStatus { returnStatus = Wont, breakStatus = Will }
lineFlowStatus Locate { value = Return {} } = return $ FlowStatus { returnStatus = Will, breakStatus = Wont }
lineFlowStatus Locate { value = While { whileBody = body } } = do
	FlowStatus { returnStatus = status } <- blockFlowStatus body
	return $ FlowStatus { returnStatus = status, breakStatus = Wont }
lineFlowStatus Locate { value = If { ifThenBody = thenBody, ifElseBody = elseBody } } = do
	thenFlow <- blockFlowStatus thenBody
	elseFlow <- blockFlowStatus elseBody
	return $ mergeEitherState thenFlow elseFlow
lineFlowStatus _ = return $ FlowStatus { returnStatus = Wont, breakStatus = Wont }

unreachableAfter :: FlowStatus -> Bool
unreachableAfter (FlowStatus r b) = r == Will || b == Will

blockFlowStatus' :: FlowStatus -> [Statement] -> Verify FlowStatus
blockFlowStatus' state [] = return state
blockFlowStatus' state (first:rest)
	|unreachableAfter state = Left $ Locate (at first) $ Message $ "statement is unreachable"
	|otherwise = do
		firstState <- lineFlowStatus first
		blockFlowStatus' (mergeSeqState state firstState) rest

-- this is the one that's principally exposed
blockFlowStatus :: [Statement] -> Verify FlowStatus
blockFlowStatus code = blockFlowStatus' (FlowStatus {returnStatus = Wont, breakStatus = Wont}) code

-- code, isVoid
methodBodyFlowVerify :: [Statement] -> Bool -> Verify ()
methodBodyFlowVerify code isVoid = do
	status <- blockFlowStatus code
	if breakStatus status /= Wont then Left $ Locate (Special "*") $ Message $ "methods cannot have a top-level break"
		else return ()
	if not isVoid && returnStatus status /= Will then Left $ Locate (Special "*") $ Message $ "non-void methods must return a value"
		else return ()
	return ()

