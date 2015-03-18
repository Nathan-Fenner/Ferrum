
import Parse.Core
import Parse.Expression
import Parse.Statement
import Location
import Lex



main = do
	source <- readFile "test.txt"
	print $ map value $ lexer "test.txt" source