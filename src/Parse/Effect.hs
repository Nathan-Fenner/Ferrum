
module Parse.Effect where

import Message
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
	discardAt <- expectAt (TSpecial "discards") $ Message $ "expected keyword `discards`"
	expr <- parseExpression
	return $ Locate discardAt $ Discard expr

parseEffectModify :: Parse Effect
parseEffectModify = do
	modifyAt <- expectAt (TSpecial "modifies") $ Message $ "expected keyword `modifies`"
	expr <- parseExpression
	return $ Locate modifyAt $ Modify expr

parseEffectAlter :: Parse Effect
parseEffectAlter = do
	alterAt <- expectAt (TSpecial "alters") $ Message $ "expected keyword `alters`"
	expr <- parseExpression
	return $ Locate alterAt $ Alter expr

parseEffectRetain :: Parse Effect
parseEffectRetain = do
	retainAt <- expectAt (TSpecial "retains") $ Message $ "expected keyword `retains`"
	expr <- parseExpression
	expect (TSpecial "in") $ Message $ "expected keyword `in` to follow keyword `retains` at " ++ displayLocation retainAt
	from <- parseExpression
	return $ Locate retainAt $ Retain expr from

parseEffectEffect :: Parse Effect
parseEffectEffect = do
	effectAt <- expectAt (TSpecial "effects") $ Message $ "expected keyword `effects`"
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

parseEffectsUntil :: Parse Bool -> Parse [Effect]
parseEffectsUntil ender = do
	r <- ender
	case r of
		True -> return []
		False -> do
			first <- parseEffect
			rest <- manyUntil ender (do
				expect (TSpecial ",") $ Message $ "expected `,` to follow effect"
				parseEffect
				)
			return $ first : rest