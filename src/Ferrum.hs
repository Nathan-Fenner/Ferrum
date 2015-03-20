
import Parse.Core
import Parse.Expression
import Parse.Statement
import Parse.Type
import Parse.Effect
import Parse.Class
import Parse.Module
import Location
import Lex


main :: IO ()
main = do
	source <- readFile "test.txt"
	let lexed = lexer "test.txt" source
	putStrLn $ concat $ map (++ "\n") $ map show lexed
	report parseModule $ lexer "test.txt" source
	return ()

report :: Show x => Parse x -> [Locate Token] -> IO ()
report parser tokens = case run parser tokens of
	(_, Right result) -> print result
	(count, Left (Message message)) -> print $ (displayLocation $ at $ head $ drop count tokens) ++ "   " ++ message
