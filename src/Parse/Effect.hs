
module Parse.Effect where

import Parse.Core
import Parse.Expression
import Lex
import Location

data EffectValue
	= Discard Expression
	| Modify Expression
	| Alter Expression
	| Retain Expression Expression
	| Effect Expression
	deriving Show

type Effect = Locate EffectValue

parseEffectDiscard :: Parse Effect
parseEffectDiscard = do
	discardAt <- expect (TSpecial "discards") $ Message $ "expected keyword `discards`"
	expr <- parseExpression
	return $ Locate discardAt $ Discard expr

parseEffectModify :: Parse Effect
parseEffectModify = do
	modifyAt <- expect (TSpecial "modifies") $ Message $ "expected keyword `modifies`"
	expr <- parseExpression
	return $ Locate modifyAt $ Modify expr

parseEffectAlter :: Parse Effect
parseEffectAlter = do
	alterAt <- expect (TSpecial "alters") $ Message $ "expected keyword `alters`"
	expr <- parseExpression
	return $ Locate alterAt $ Alter expr

parseEffectRetain :: Parse Effect
parseEffectRetain = do
	retainAt <- expect (TSpecial "retains") $ Message $ "expected keyword `retains`"
	expr <- parseExpression
	expect (TSpecial "in") $ Message $ "expected keyword `in` to follow keyword `retains` at " ++ displayLocation retainAt
	from <- parseExpression
	return $ Locate retainAt $ Retain expr from

parseEffectEffect :: Parse Effect
parseEffectEffect = do
	effectAt <- expect (TSpecial "effects") $ Message $ "expected keyword `effects`"
	expr <- parseExpression
	return $ Locate effectAt $ Effect expr

parseEffect :: Parse Effect
parseEffect = do
	next <- peekMaybe
	case fmap value next of
		Just (TSpecial "discards") -> parseEffectDiscard
		Just (TSpecial "modifies") -> parseEffectModify
		Just (TSpecial "alters") -> parseEffectAlter
		Just (TSpecial "retains") -> parseEffectRetain
		Just (TSpecial "effects") -> parseEffectEffect
		_ -> crash $ Message $ "effect expected"