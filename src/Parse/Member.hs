
module Parse.Member where

import Message
import Parse.Core
import Parse.Type
import Parse.Expression
import Parse.Statement
import Parse.Effect
import Parse.Modifier
import Location
import Lex

data Visibility
	= Public
	| Private
	| Protected
	deriving Show

parseVisibility :: Parse Visibility
parseVisibility = do
	next <- peekMaybe
	case next of
		Just (Locate _ (TSpecial "public")) -> advance 1 >> return Public
		Just (Locate _ (TSpecial "private")) -> advance 1 >> return Private
		Just (Locate _ (TSpecial "protected")) -> advance 1 >> return Protected
		_ -> crash $ Message $ "expected `public` or `private` or `protected` to indicate member visibility"

data Member = Member {memberVisibility :: Visibility, memberValue :: MemberValue}
	deriving Show

parseMember :: Parse Member
parseMember = do
	visibility <- parseVisibility
	next <- peekMaybe
	member <- case next of
		Just (Locate _ (TSpecial "field")) -> parseField
		Just (Locate _ (TSpecial "method")) -> parseMethod
		Just (Locate _ (TSpecial "constructor")) -> parseConstructor
		_ -> crash $ Message $ "expected `field` or `method` or `constructor` to follow visibility to indicate member form"
	return $ Member visibility member

data MemberValue
	= Field
		{ fieldModifier :: Modifier
		, fieldType :: Type
		, fieldName :: Name
		}
	| Method
		{ methodReturnType :: Type
		, methodFromExpression :: (Maybe Expression)
		, methodName :: Name
		, methodArguments :: [(Type, Name)]
		, methodEffects :: [Effect]
		, methodBody :: [Statement]
		}
	| Constructor
		{ constructorArguments :: [(Type, Name)]
		, constructorEffects :: [Effect]
		, constructorBody :: [Statement]
		}
	deriving Show

parseField :: Parse MemberValue
parseField = do
	fieldAt <- expectAt (TSpecial "field") $ Message $ "expected keyword `field` to begin field"
	modifier <- parseModifier
	myFieldType <- parseType
	expect (TSpecial ":") $ Message $ "expected `:` to follow type in field starting at " ++ displayLocation fieldAt
	Locate nameAt name <- expectName $ Message $ "expected name to follow `field` and type"
	expect (TSpecial ";") $ Message $ "expected `;` to follow field name `" ++ name ++ "`"
	return $ Field modifier myFieldType (Locate nameAt name)

parseFrom :: Parse (Maybe Expression)
parseFrom = do
	next <- peekMaybe
	case next of
		Just (Locate _fromAt (TSpecial "from")) -> do
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
	openAt <- expectAt (TSpecial "(") $ Message $ "expected `(` to begin argument list"
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
			expect (TSpecial ")") $ Message $ "expected `)` to close argument list begun at " ++ displayLocation openAt
			return $ first : rest

parseMethod :: Parse MemberValue
parseMethod = do
	expect (TSpecial "method") $ Message $ "expected keyword `method` to begin method"
	returnType <- parseType
	from <- parseFrom
	expect (TSpecial ":") $ Message $ "expected `:` to follow return type"
	Locate nameAt name <- expectName $ Message $ "expected method name to follow return type"
	args <- parseArguments
	effects <- parseEffectsUntil (checkNext (TSpecial "{"))
	body <- parseBody $ Message $ "expected function body for method `" ++ name ++ "` at " ++ displayLocation nameAt
	return $ Method returnType from (Locate nameAt name) args effects body

parseGenerics :: Parse [Name]
parseGenerics = do
	next <- peekMaybe
	case next of
		Just (Locate openAt (TSpecial "[")) -> do
			advance 1 -- skip it
			first <- expectName $ Message $ "expected generic type name to follow `[` at " ++ displayLocation openAt
			rest <- manyUntil (checkNext (TSpecial "]"))
				(do
					expect (TSpecial ",") $ Message $ "expected `,` to separate types in generic list starting at " ++ displayLocation openAt
					expectName $ Message $ "expected generic type name to follow `,` in generic list started at " ++ displayLocation openAt
				)
			expect (TSpecial "]") $ Message $ "expected `]` to close `[` at " ++ displayLocation openAt
			return $ first : rest
		_ -> return []

parseConstructor :: Parse MemberValue
parseConstructor = do
	constructorAt <- expectAt (TSpecial "constructor") $ Message $ "expected keyword `constructor` to begin constructor"
	arguments <- parseArguments
	effects <- parseEffectsUntil (checkNext (TSpecial "{"))
	body <- parseBody $ Message $ "expected function body for constructor at " ++ displayLocation constructorAt
	return $ Constructor arguments effects body

