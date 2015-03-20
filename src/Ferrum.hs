
import Parse.Core
import Parse.Expression
import Parse.Statement
import Parse.Type
import Parse.Effect
import Parse.Class
import Location
import Lex



main = do
	source <- readFile "test.txt"
	print $ run (parseBody (Message "must begin with `{`")) $ lexer "test.txt" source