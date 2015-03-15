
import Parse.Core
import Parse.Expression
import Location
import Lex



main = do
	source <- readFile "test.txt"
	print $ lexer "test.txt" source