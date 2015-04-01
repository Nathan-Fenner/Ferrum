
module Parse.Class where

import Syntax.Class
import Message
import Parse.Core
import Parse.Member
import Location
import Lex



parseClass :: Parse Class
parseClass = do
	expect (TSpecial "class") $ Message $ "expected keyword `class` to begin class definition"
	Locate nameAt name <- expectName $ Message $ "expected class name"
	generics <- parseGenerics
	expect (TSpecial "{") $ Message $ "expected `{` to follow class head for `" ++ name ++ "` to begin class body"
	members <- manyUntil (checkNext (TSpecial "}")) parseMember
	expect (TSpecial "}") $ Message $ "expected `}` to end class definition for `" ++ name ++ "` defined at " ++ displayLocation nameAt
	return $ Class (Locate nameAt name) generics members