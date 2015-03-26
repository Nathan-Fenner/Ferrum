
{-# OPTIONS_GHC -w #-}

import Parse.Core
import Parse.Module
import Location
import Lex

import Verify
import Verify.Type
import Verify.Type.Form
import Verify.Class
import Verify.Member
import Verify.Statement

main :: IO ()
main = do
	source <- readFile "test.txt"
	let lexed = lexer "test.txt" source
	putStrLn $ concat $ map (++ "\n") $ map show lexed
	report parseModule $ lexer "test.txt" source
	return ()

report :: Parse Module -> [Locate Token] -> IO ()
report parser tokens = case run parser tokens of
	(_, Right (Module _ result)) -> do
		print result
		putStrLn $ "Verifying class:"
		print $ map verifyClass result
	(count, Left (Message message)) -> do
		print $ (displayLocation $ at $ head $ drop count tokens) ++ "   " ++ message
		putStrLn $ concat $ map (++"\n") $ map show $ take 10 $ drop count tokens

