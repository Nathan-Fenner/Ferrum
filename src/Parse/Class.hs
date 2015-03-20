
module Parse.Class where

import Parse.Core
import Parse.Type
import Parse.Expression
import Parse.Statement
import Parse.Effect
import Parse.Modifier
import Location
import Lex

data Field
	= Field Modifier Type Name
	deriving Show

parseField :: Parse Field
parseField = do
	fieldAt <- expect (TSpecial "field") $ Message $ "expected keyword `field` to begin field"
	modifier <- parseModifier
	fieldType <- parseType
	expect (TSpecial ":") $ Message $ "expected `:` to follow type in field starting at " ++ displayLocation fieldAt
	Locate nameAt name <- expectName $ Message $ "expected name to follow `field` and type"
	expect (TSpecial ";") $ Message $ "expected `;` to follow field name `" ++ name ++ "`"
	return $ Field modifier fieldType (Locate nameAt name)

data Method
	= Method Type (Maybe Expression) Name [(Type, Name)] [Effect] [Statement]
	deriving Show

parseFrom :: Parse (Maybe Expression)
parseFrom = do
	next <- peekMaybe
	case next of
		Just (Locate fromAt (TSpecial "from")) -> do
			advance 1 -- skip the from
			fmap Just parseExpression
		_ -> return Nothing

parseArgument :: Parse (Type, Name)
parseArgument = do
	argumentType <- parseType
	expect (TSpecial ":") $ Message $ "expected `:` to follow type declaration in argument"
	name <- expectName $ Message $ "expected name to follow type in argument"
	return $ (argumentType, name)

parseArguments :: Parse [(Type, Name)]
parseArguments = do
	expect (TSpecial "(") $ Message $ "expected `(` to begin argument list"
	next <- peekMaybe
	case next of
		Just (Locate _closeAt  (TSpecial ")")) -> do
			advance 1 -- skip the `)`
			return []
		_ -> do
			first <- parseArgument
			rest <- manyUntil
				(checkNext (TSpecial ")"))
				(do
					expect (TSpecial ",") $ Message $ "expected `,` to follow argument in argument list"
					parseArgument
				)
			return $ first : rest

parseMethod = do
	methodAt <- expect (TSpecial "method") $ Message $ "expected keyword `method` to begin method"
	returnType <- parseType
	from <- parseFrom
	Locate nameAt name <- expectName $ Message $ "expected method name to follow return type"
	args <- parseArguments
	effects <- parseEffectsUntil (checkNext (TSpecial "{"))
	body <- parseBody $ Message $ "expected function body for method `" ++ name ++ "` at " ++ displayLocation nameAt
	return $ Method returnType from (Locate nameAt name) args effects body