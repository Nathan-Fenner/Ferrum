
module Parse.Class where
import Parse.Core
import Parse.Member
import Parse.Type
import Location
import Lex

data Class = Class Name [Type] [Member] deriving Show

parseClass :: Parse Class
parseClass = do
	expect (TSpecial "class") $ Message $ "expected keyword `class` to begin class definition"
	Locate nameAt name <- expectName $ Message $ "expected class name"
	generics <- parseGenerics
	expect (TSpecial "{") $ Message $ "expected `{` to follow class head for `" ++ name ++ "` to begin class body"
	members <- manyUntil (checkNext (TSpecial "}")) parseMember
	return $ Class (Locate nameAt name) generics members